/* Copyright (c) 2026, Emily Pillmore. BSD-3-Clause; see LICENSE. */

#ifndef HS_BASE16_SIMD_H
#define HS_BASE16_SIMD_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Process complete vector blocks at the start of a buffer. The caller handles
 * the remaining input and error reporting. Source and destination must not
 * overlap; source has len readable bytes and destination has room for the
 * produced prefix. Loads never extend past len, and stores cover only the
 * returned prefix. Buffers must remain alive throughout the call.
 *
 * Encode emits lowercase ASCII and writes exactly 2 * result bytes.
 * Decode accepts ASCII 0-9, a-f and A-F; it returns an even count and writes
 * exactly result / 2 bytes. An entire vector block containing an invalid byte
 * is left untouched, including its destination, so the caller can report the
 * first invalid byte. The final incomplete block is also left to the caller.
 *
 * Unsupported targets return zero without reading or writing any buffers.
 * The caller must reject output-size overflow before allocating buffers.
 */
size_t hs_base16_simd_encode(const uint8_t *src, size_t len, uint8_t *dst);
size_t hs_base16_simd_decode(const uint8_t *src, size_t len, uint8_t *dst);

/* 0: scalar fallback; 1: x86-64 SSSE3; 2: ARM64 NEON; 3: x86-64 AVX2. */
int hs_base16_simd_backend(void);

#ifdef __cplusplus
}
#endif

#endif /* HS_BASE16_SIMD_H */
