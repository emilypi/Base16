/* Copyright (c) 2026, Emily Pillmore. BSD-3-Clause; see LICENSE. */

/*
 * Standalone differential/bounds checks; link with cbits/base16_simd.c.
 * Recommended: -std=c99 -O2 -Wall -Wextra -Werror
 *              -fsanitize=address,undefined -fno-omit-frame-pointer
 * Repeat with HS_BASE16_SIMD_FORCE_SCALAR and HS_BASE16_SIMD_FORCE_SSSE3.
 */
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(__unix__) || defined(__APPLE__)
#include <sys/mman.h>
#include <unistd.h>
#define HS_BASE16_GUARD_PAGES 1
#endif

#include "base16_simd.h"

#define CHECK(condition) do { \
    if (!(condition)) { \
        fprintf(stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, #condition); \
        exit(EXIT_FAILURE); \
    } \
} while (0)

static const uint8_t alphabet[] = "0123456789abcdef";
static size_t encode_block;
static size_t decode_block;
static size_t checks;

/* Deliberately scalar and independent of the kernels' vector classification. */
static int hex_value(uint8_t value)
{
    if (value >= '0' && value <= '9') {
        return value - '0';
    }
    if (value >= 'a' && value <= 'f') {
        return value - 'a' + 10;
    }
    if (value >= 'A' && value <= 'F') {
        return value - 'A' + 10;
    }
    return -1;
}

static size_t encode_prefix(size_t len)
{
    return encode_block == 0 ? 0 : len / encode_block * encode_block;
}

static size_t decode_prefix(const uint8_t *src, size_t len)
{
    size_t consumed = 0;

    if (decode_block == 0) {
        return 0;
    }
    while (len - consumed >= decode_block) {
        for (size_t i = 0; i < decode_block; i++) {
            if (hex_value(src[consumed + i]) < 0) {
                return consumed;
            }
        }
        consumed += decode_block;
    }
    return consumed;
}

static void scalar_encode(const uint8_t *src, size_t len, uint8_t *dst)
{
    for (size_t i = 0; i < len; i++) {
        *dst++ = alphabet[src[i] / 16];
        *dst++ = alphabet[src[i] % 16];
    }
}

static void scalar_decode(const uint8_t *src, size_t len, uint8_t *dst)
{
    for (size_t i = 0; i < len; i += 2) {
        *dst++ = (uint8_t)(hex_value(src[i]) * 16 + hex_value(src[i + 1]));
    }
}

static void check_canary(const uint8_t *data, size_t len)
{
    for (size_t i = 0; i < len; i++) {
        CHECK(data[i] == 0xa5);
    }
}

static void check_encode(const uint8_t *src, size_t len, size_t offset)
{
    const size_t expected = encode_prefix(len);
    const size_t output_len = expected * 2;
    uint8_t *output = malloc(offset + output_len + 32);
    uint8_t *reference = malloc(output_len + 1);
    size_t consumed;

    CHECK(output != NULL && reference != NULL);
    memset(output, 0xa5, offset + output_len + 32);
    scalar_encode(src, expected, reference);
    consumed = hs_base16_simd_encode(src, len, output + offset);
    CHECK(consumed == expected && consumed <= len);
    CHECK(memcmp(output + offset, reference, output_len) == 0);
    check_canary(output, offset);
    check_canary(output + offset + output_len, 32);
    free(reference);
    free(output);
    checks++;
}

static void check_decode(const uint8_t *src, size_t len, size_t offset)
{
    const size_t expected = decode_prefix(src, len);
    const size_t output_len = expected / 2;
    uint8_t *output = malloc(offset + output_len + 32);
    uint8_t *reference = malloc(output_len + 1);
    size_t consumed;

    CHECK(output != NULL && reference != NULL);
    memset(output, 0xa5, offset + output_len + 32);
    scalar_decode(src, expected, reference);
    consumed = hs_base16_simd_decode(src, len, output + offset);
    CHECK(consumed == expected);
    CHECK(consumed % 2 == 0 && consumed <= len);
    CHECK(memcmp(output + offset, reference, output_len) == 0);
    check_canary(output, offset);
    check_canary(output + offset + output_len, 32);
    free(reference);
    free(output);
    checks++;
}

static void test_lengths_and_alignment(void)
{
    uint8_t source[1056];
    uint8_t encoded[2112];

    for (size_t offset = 0; offset < 32; offset++) {
        for (size_t i = 0; i < sizeof(source); i++) {
            source[i] = (uint8_t)(i * 73 + offset * 19);
        }
        scalar_encode(source, sizeof(source), encoded);
        for (size_t len = 0; len <= 1024; len++) {
            check_encode(source + offset, len, 31 - offset);
            check_decode(encoded + offset, len, 31 - offset);
        }
    }
    /* Vary both alignments independently at every relevant vector boundary. */
    const size_t sizes[] = {0, 1, 15, 16, 17, 31, 32, 33, 63, 64, 65,
                            127, 128, 129, 255, 256, 257};
    for (size_t source_offset = 0; source_offset < 32; source_offset++) {
        for (size_t output_offset = 0; output_offset < 32; output_offset++) {
            for (size_t i = 0; i < sizeof(sizes) / sizeof(sizes[0]); i++) {
                check_encode(source + source_offset, sizes[i], output_offset);
                check_decode(encoded + source_offset, sizes[i], output_offset);
            }
        }
    }
}

static void test_all_byte_values(void)
{
    uint8_t source[257];
    uint8_t encoded[514];

    for (unsigned value = 0; value <= 255; value++) {
        memset(source, (int)value, sizeof(source));
        check_encode(source, sizeof(source), value % 32);
        scalar_encode(source, sizeof(source), encoded);
        check_decode(encoded, sizeof(encoded), value % 32);
        for (size_t i = 0; i < sizeof(encoded); i++) {
            if (encoded[i] >= 'a' && encoded[i] <= 'f' && i % 2 == 0) {
                encoded[i] = (uint8_t)(encoded[i] - 'a' + 'A');
            }
        }
        check_decode(encoded, sizeof(encoded), value % 32);
    }
}

static void test_invalid_block_stops(void)
{
    uint8_t source[580];

    for (size_t position = 0; position < sizeof(source); position++) {
        for (unsigned value = 0; value <= 255; value++) {
            memset(source, 'a', sizeof(source));
            source[position] = (uint8_t)value;
            check_decode(source, sizeof(source), position % 32);
        }
    }
}

static uint32_t next_random(uint32_t *state)
{
    uint32_t value = *state;
    value ^= value << 13;
    value ^= value >> 17;
    value ^= value << 5;
    *state = value;
    return value;
}

static void test_random_mixed_inputs(void)
{
    uint8_t source[4096];
    uint8_t encoded[8192];
    uint32_t state = UINT32_C(0x6d253fb9);

    for (size_t sample = 0; sample < 256; sample++) {
        const size_t len = next_random(&state) % (sizeof(source) + 1);
        const size_t encoded_len = len * 2;
        const size_t offset = next_random(&state) % 32;

        for (size_t i = 0; i < len; i++) {
            source[i] = (uint8_t)next_random(&state);
        }
        check_encode(source, len, offset);
        scalar_encode(source, len, encoded);
        for (size_t i = 0; i < encoded_len; i++) {
            if (encoded[i] >= 'a' && encoded[i] <= 'f' && next_random(&state) % 2) {
                encoded[i] = (uint8_t)(encoded[i] - 'a' + 'A');
            }
        }
        check_decode(encoded, encoded_len, offset);
        if (encoded_len > 0) {
            const size_t position = next_random(&state) % encoded_len;
            encoded[position] = (uint8_t)next_random(&state);
            check_decode(encoded, encoded_len, offset);
        }
    }
}

static void test_tight_allocations(void)
{
    for (size_t len = 1; len <= 1024; len++) {
        uint8_t *source = malloc(len);
        const size_t encode_len = encode_prefix(len) * 2;
        uint8_t *encoded = malloc(encode_len == 0 ? 1 : encode_len);
        size_t decode_len;
        uint8_t *decoded;

        CHECK(source != NULL && encoded != NULL);
        memset(source, 0xfb, len);
        CHECK(hs_base16_simd_encode(source, len, encoded) == encode_prefix(len));
        memset(source, 'F', len);
        decode_len = decode_prefix(source, len) / 2;
        decoded = malloc(decode_len == 0 ? 1 : decode_len);
        CHECK(decoded != NULL);
        CHECK(hs_base16_simd_decode(source, len, decoded) == decode_prefix(source, len));
        free(decoded);
        free(encoded);
        free(source);
        checks += 2;
    }
}

#if defined(HS_BASE16_GUARD_PAGES)
static uint8_t *guarded_buffer(size_t page_size)
{
    uint8_t *mapping = mmap(NULL, page_size * 3, PROT_NONE,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    CHECK(mapping != MAP_FAILED);
    CHECK(mprotect(mapping + page_size, page_size, PROT_READ | PROT_WRITE) == 0);
    return mapping;
}

static void test_guard_pages(void)
{
    const long raw_page_size = sysconf(_SC_PAGESIZE);
    size_t page_size;
    uint8_t *source_mapping;
    uint8_t *output_mapping;

    CHECK(raw_page_size >= 2048);
    page_size = (size_t)raw_page_size;
    source_mapping = guarded_buffer(page_size);
    output_mapping = guarded_buffer(page_size);

    for (size_t len = 0; len <= 1024; len++) {
        for (size_t at_end = 0; at_end < 2; at_end++) {
            uint8_t *source = source_mapping + page_size +
                              (at_end ? page_size - len : 0);
            const size_t encode_len = encode_prefix(len) * 2;
            uint8_t *output = output_mapping + page_size +
                              (at_end ? page_size - encode_len : 0);
            size_t expected;
            size_t decode_len;

            memset(source, 0xfb, len);
            CHECK(hs_base16_simd_encode(source, len, output) == encode_prefix(len));
            memset(source, 'F', len);
            expected = decode_prefix(source, len);
            decode_len = expected / 2;
            output = output_mapping + page_size + (at_end ? page_size - decode_len : 0);
            CHECK(hs_base16_simd_decode(source, len, output) == expected);
            checks += 2;
        }
    }
    CHECK(munmap(source_mapping, page_size * 3) == 0);
    CHECK(munmap(output_mapping, page_size * 3) == 0);
}
#endif

int main(int argc, char **argv)
{
    const int backend = hs_base16_simd_backend();

    if (argc != 1) {
        if (argc != 3 || strcmp(argv[1], "--require-backend") != 0 ||
            strlen(argv[2]) != 1 || argv[2][0] < '0' || argv[2][0] > '3') {
            fprintf(stderr, "usage: %s [--require-backend 0|1|2|3]\n", argv[0]);
            return EXIT_FAILURE;
        }
        if (backend != argv[2][0] - '0') {
            fprintf(stderr, "required SIMD backend %c, detected %d\n", argv[2][0], backend);
            return EXIT_FAILURE;
        }
    }
    CHECK(backend >= 0 && backend <= 3);
#if defined(HS_BASE16_SIMD_FORCE_SCALAR)
    CHECK(backend == 0);
#elif defined(HS_BASE16_SIMD_FORCE_SSSE3) && defined(__x86_64__)
    CHECK(backend == 0 || backend == 1);
#elif defined(__aarch64__) && defined(__ARM_NEON) && \
      (defined(__GNUC__) || defined(__clang__))
    CHECK(backend == 2);
#endif
    encode_block = backend == 3 ? 32 : backend == 0 ? 0 : 16;
    decode_block = backend == 1 ? 16 : backend == 0 ? 0 : 32;
    printf("SIMD backend: %d (%s)\n", backend,
           backend == 3 ? "AVX2" : backend == 2 ? "NEON" :
           backend == 1 ? "SSSE3" : "scalar fallback");
    /* Zero-length and unsupported calls must not dereference their pointers. */
    CHECK(hs_base16_simd_encode(NULL, 0, NULL) == 0);
    CHECK(hs_base16_simd_decode(NULL, 0, NULL) == 0);
    if (backend == 0) {
        CHECK(hs_base16_simd_encode(NULL, SIZE_MAX, NULL) == 0);
        CHECK(hs_base16_simd_decode(NULL, SIZE_MAX, NULL) == 0);
    }
    test_lengths_and_alignment();
    test_all_byte_values();
    test_invalid_block_stops();
    test_random_mixed_inputs();
    test_tight_allocations();
#if defined(HS_BASE16_GUARD_PAGES)
    test_guard_pages();
#endif
    printf("Passed %zu differential/bounds checks.\n", checks);
    return EXIT_SUCCESS;
}
