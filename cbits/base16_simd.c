/* Copyright (c) 2026, Emily Pillmore. BSD-3-Clause; see LICENSE. */

#include "base16_simd.h"

/*
 * Original bounded prefix kernels. Target attributes keep x86 instructions
 * out of baseline dispatch, with no global ISA flags or mutable state.
 * HS_BASE16_SIMD_FORCE_SCALAR exercises the unsupported-target fallback.
 * HS_BASE16_SIMD_FORCE_SSSE3 caps x86 dispatch for older-processor testing.
 */
#if !defined(HS_BASE16_SIMD_FORCE_SCALAR)
#if defined(__aarch64__) && defined(__ARM_NEON) && \
    (defined(__GNUC__) || defined(__clang__))
#define HS_BASE16_NEON 1
#include <arm_neon.h>
#elif defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))
#define HS_BASE16_SSSE3 1
#include <immintrin.h>
#define HS_BASE16_TARGET_SSSE3 __attribute__((target("ssse3")))
#if !defined(HS_BASE16_SIMD_FORCE_SSSE3)
#define HS_BASE16_AVX2 1
#define HS_BASE16_TARGET_AVX2 __attribute__((target("avx2")))
#endif
#endif
#endif

#if defined(HS_BASE16_NEON) || defined(HS_BASE16_SSSE3)
static const uint8_t hs_base16_alphabet[] = "0123456789abcdef";
#endif

#if defined(HS_BASE16_NEON)

static size_t hs_base16_neon_encode(const uint8_t *src, size_t len, uint8_t *dst)
{
    size_t consumed = 0;
    const uint8x16_t alphabet = vld1q_u8(hs_base16_alphabet);
    const uint8x16_t mask = vdupq_n_u8(15);

    while (len - consumed >= 16) {
        const uint8x16_t input = vld1q_u8(src + consumed);
        uint8x16x2_t output;

        output.val[0] = vqtbl1q_u8(alphabet, vshrq_n_u8(input, 4));
        output.val[1] = vqtbl1q_u8(alphabet, vandq_u8(input, mask));
        vst2q_u8(dst, output);
        consumed += 16;
        dst += 32;
    }
    return consumed;
}

static inline uint8x16_t hs_base16_neon_nibbles(uint8x16_t input,
                                               uint8x16_t *valid)
{
    const uint8x16_t digit = vsubq_u8(input, vdupq_n_u8('0'));
    const uint8x16_t letter = vsubq_u8(vorrq_u8(input, vdupq_n_u8(0x20)),
                                      vdupq_n_u8('a'));
    const uint8x16_t is_digit = vcleq_u8(digit, vdupq_n_u8(9));
    const uint8x16_t is_letter = vcleq_u8(letter, vdupq_n_u8(5));

    *valid = vorrq_u8(is_digit, is_letter);
    return vbslq_u8(is_letter, vaddq_u8(letter, vdupq_n_u8(10)), digit);
}

static size_t hs_base16_neon_decode(const uint8_t *src, size_t len, uint8_t *dst)
{
    size_t consumed = 0;

    while (len - consumed >= 32) {
        const uint8x16x2_t input = vld2q_u8(src + consumed);
        uint8x16_t valid_hi, valid_lo;
        const uint8x16_t hi = hs_base16_neon_nibbles(input.val[0], &valid_hi);
        const uint8x16_t lo = hs_base16_neon_nibbles(input.val[1], &valid_lo);

        /* Validate the whole block before committing even its first byte. */
        if (vminvq_u8(vandq_u8(valid_hi, valid_lo)) == 0) {
            break;
        }
        vst1q_u8(dst, vorrq_u8(vshlq_n_u8(hi, 4), lo));
        consumed += 32;
        dst += 16;
    }
    return consumed;
}

#elif defined(HS_BASE16_SSSE3)

HS_BASE16_TARGET_SSSE3
static size_t hs_base16_ssse3_encode(const uint8_t *src, size_t len, uint8_t *dst)
{
    size_t consumed = 0;
    const __m128i alphabet = _mm_loadu_si128((const __m128i *)hs_base16_alphabet);
    const __m128i mask = _mm_set1_epi8(15);

    while (len - consumed >= 16) {
        const __m128i input = _mm_loadu_si128((const __m128i *)(src + consumed));
        const __m128i hi = _mm_shuffle_epi8(alphabet,
            _mm_and_si128(_mm_srli_epi16(input, 4), mask));
        const __m128i lo = _mm_shuffle_epi8(alphabet, _mm_and_si128(input, mask));

        _mm_storeu_si128((__m128i *)dst, _mm_unpacklo_epi8(hi, lo));
        _mm_storeu_si128((__m128i *)(dst + 16), _mm_unpackhi_epi8(hi, lo));
        consumed += 16;
        dst += 32;
    }
    return consumed;
}

HS_BASE16_TARGET_SSSE3
static inline __m128i hs_base16_ssse3_nibbles(__m128i input, __m128i *valid)
{
    const __m128i lower = _mm_or_si128(input, _mm_set1_epi8(0x20));
    const __m128i is_digit = _mm_and_si128(
        _mm_cmpgt_epi8(input, _mm_set1_epi8('0' - 1)),
        _mm_cmpgt_epi8(_mm_set1_epi8('9' + 1), input));
    const __m128i is_letter = _mm_and_si128(
        _mm_cmpgt_epi8(lower, _mm_set1_epi8('a' - 1)),
        _mm_cmpgt_epi8(_mm_set1_epi8('f' + 1), lower));

    /* Bytes >= 128 fail the signed lower-bound comparisons in both ranges. */
    *valid = _mm_or_si128(is_digit, is_letter);
    return _mm_add_epi8(_mm_and_si128(input, _mm_set1_epi8(15)),
                        _mm_and_si128(is_letter, _mm_set1_epi8(9)));
}

HS_BASE16_TARGET_SSSE3
static size_t hs_base16_ssse3_decode(const uint8_t *src, size_t len, uint8_t *dst)
{
    size_t consumed = 0;

    while (len - consumed >= 16) {
        const __m128i input = _mm_loadu_si128((const __m128i *)(src + consumed));
        __m128i valid;
        const __m128i value = hs_base16_ssse3_nibbles(input, &valid);
        __m128i pairs;
        __m128i output;

        if (_mm_movemask_epi8(valid) != 0xffff) {
            break;
        }
        /* Adjacent nibbles form unsigned 0..255 words, without saturation. */
        pairs = _mm_maddubs_epi16(value, _mm_set1_epi16(0x0110));
        output = _mm_packus_epi16(pairs, _mm_setzero_si128());
        _mm_storel_epi64((__m128i *)dst, output);
        consumed += 16;
        dst += 8;
    }
    return consumed;
}

#if defined(HS_BASE16_AVX2)

HS_BASE16_TARGET_AVX2
static size_t hs_base16_avx2_encode(const uint8_t *src, size_t len, uint8_t *dst)
{
    size_t consumed = 0;
    const __m256i alphabet = _mm256_broadcastsi128_si256(
        _mm_loadu_si128((const __m128i *)hs_base16_alphabet));
    const __m256i mask = _mm256_set1_epi8(15);

    while (len - consumed >= 32) {
        const __m256i input = _mm256_loadu_si256((const __m256i *)(src + consumed));
        const __m256i hi = _mm256_shuffle_epi8(alphabet,
            _mm256_and_si256(_mm256_srli_epi16(input, 4), mask));
        const __m256i lo = _mm256_shuffle_epi8(alphabet, _mm256_and_si256(input, mask));
        const __m256i first = _mm256_unpacklo_epi8(hi, lo);
        const __m256i second = _mm256_unpackhi_epi8(hi, lo);

        /* Unpacks operate within 128-bit lanes; restore original byte order. */
        _mm256_storeu_si256((__m256i *)dst,
            _mm256_permute2x128_si256(first, second, 0x20));
        _mm256_storeu_si256((__m256i *)(dst + 32),
            _mm256_permute2x128_si256(first, second, 0x31));
        consumed += 32;
        dst += 64;
    }
    return consumed;
}

HS_BASE16_TARGET_AVX2
static size_t hs_base16_avx2_decode(const uint8_t *src, size_t len, uint8_t *dst)
{
    size_t consumed = 0;

    while (len - consumed >= 32) {
        const __m256i input = _mm256_loadu_si256((const __m256i *)(src + consumed));
        const __m256i lower = _mm256_or_si256(input, _mm256_set1_epi8(0x20));
        const __m256i is_digit = _mm256_and_si256(
            _mm256_cmpgt_epi8(input, _mm256_set1_epi8('0' - 1)),
            _mm256_cmpgt_epi8(_mm256_set1_epi8('9' + 1), input));
        const __m256i is_letter = _mm256_and_si256(
            _mm256_cmpgt_epi8(lower, _mm256_set1_epi8('a' - 1)),
            _mm256_cmpgt_epi8(_mm256_set1_epi8('f' + 1), lower));
        __m256i value;
        __m256i pairs;
        __m128i output;

        if (_mm256_movemask_epi8(_mm256_or_si256(is_digit, is_letter)) != -1) {
            break;
        }
        value = _mm256_add_epi8(_mm256_and_si256(input, _mm256_set1_epi8(15)),
            _mm256_and_si256(is_letter, _mm256_set1_epi8(9)));
        pairs = _mm256_maddubs_epi16(value, _mm256_set1_epi16(0x0110));
        /* Pack both lanes into exactly 16 output bytes, without lane gaps. */
        output = _mm_packus_epi16(_mm256_castsi256_si128(pairs),
                                  _mm256_extracti128_si256(pairs, 1));
        _mm_storeu_si128((__m128i *)dst, output);
        consumed += 32;
        dst += 16;
    }
    return consumed;
}

#endif /* HS_BASE16_AVX2 */
#endif

int hs_base16_simd_backend(void)
{
#if defined(HS_BASE16_NEON)
    return 2;
#elif defined(HS_BASE16_SSSE3)
#if defined(HS_BASE16_AVX2)
    /* The compiler's query also checks OS support for AVX register state. */
    if (__builtin_cpu_supports("avx2")) {
        return 3;
    }
#endif
    return __builtin_cpu_supports("ssse3") ? 1 : 0;
#else
    return 0;
#endif
}

size_t hs_base16_simd_encode(const uint8_t *src, size_t len, uint8_t *dst)
{
#if defined(HS_BASE16_NEON)
    return hs_base16_neon_encode(src, len, dst);
#elif defined(HS_BASE16_SSSE3)
    const int backend = hs_base16_simd_backend();
#if defined(HS_BASE16_AVX2)
    if (backend == 3) {
        return hs_base16_avx2_encode(src, len, dst);
    }
#endif
    if (backend == 1) {
        return hs_base16_ssse3_encode(src, len, dst);
    }
#endif
    (void)src;
    (void)len;
    (void)dst;
    return 0;
}

size_t hs_base16_simd_decode(const uint8_t *src, size_t len, uint8_t *dst)
{
#if defined(HS_BASE16_NEON)
    return hs_base16_neon_decode(src, len, dst);
#elif defined(HS_BASE16_SSSE3)
    const int backend = hs_base16_simd_backend();
#if defined(HS_BASE16_AVX2)
    if (backend == 3) {
        return hs_base16_avx2_decode(src, len, dst);
    }
#endif
    if (backend == 1) {
        return hs_base16_ssse3_decode(src, len, dst);
    }
#endif
    (void)src;
    (void)len;
    (void)dst;
    return 0;
}
