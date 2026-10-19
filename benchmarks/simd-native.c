/* Copyright (c) 2026, Emily Pillmore. BSD-3-Clause; see LICENSE. */

/* Native diagnostic benchmark; this does not measure Haskell/FFI overhead. */
#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "base16_simd.h"

static volatile size_t sink;
static const uint8_t alphabet[] = "0123456789abcdef";

static double now(void)
{
    struct timespec time;
    if (clock_gettime(CLOCK_MONOTONIC, &time) != 0) {
        perror("clock_gettime");
        exit(EXIT_FAILURE);
    }
    return (double)time.tv_sec + (double)time.tv_nsec / 1000000000.0;
}

static void encode(const uint8_t *src, size_t len, uint8_t *dst, int simd)
{
    size_t consumed = simd ? hs_base16_simd_encode(src, len, dst) : 0;
    for (size_t i = consumed; i < len; i++) {
        dst[i * 2] = alphabet[src[i] >> 4];
        dst[i * 2 + 1] = alphabet[src[i] & 15];
    }
}

static uint8_t nibble(uint8_t value)
{
    return value <= '9' ? (uint8_t)(value - '0') : (uint8_t)(value - 'a' + 10);
}

/* The benchmark only supplies independently generated valid lowercase input. */
static void decode(const uint8_t *src, size_t len, uint8_t *dst, int simd)
{
    size_t consumed = simd ? hs_base16_simd_decode(src, len, dst) : 0;
    for (size_t i = consumed; i < len; i += 2) {
        dst[i / 2] = (uint8_t)(nibble(src[i]) * 16 + nibble(src[i + 1]));
    }
}

typedef void (*operation)(const uint8_t *, size_t, uint8_t *, int);

static double measure(operation op, const uint8_t *src, size_t len,
                      uint8_t *dst, int simd, double seconds)
{
    const double start = now();
    double elapsed;
    size_t iterations = 0;

    do {
        for (size_t batch = 0; batch < 16; batch++) {
            op(src, len, dst, simd);
            sink += dst[0];
            iterations++;
        }
        elapsed = now() - start;
    } while (elapsed < seconds);
    return (double)iterations * (double)len / elapsed / 1048576.0;
}

static void usage(const char *name)
{
    fprintf(stderr, "usage: %s [--seconds 0.001..10] [--rounds 1..20] "
                    "[--pattern random|repeating]\n", name);
}

int main(int argc, char **argv)
{
    double seconds = 0.05;
    unsigned long rounds = 3;
    int random_pattern = 1;
    const size_t sizes[] = {64, 128, 1024, 16384, 1048576};
    const int backend = hs_base16_simd_backend();

    for (int i = 1; i < argc; i += 2) {
        char *end;
        if (i + 1 >= argc) {
            usage(argv[0]);
            return EXIT_FAILURE;
        }
        errno = 0;
        if (strcmp(argv[i], "--seconds") == 0) {
            seconds = strtod(argv[i + 1], &end);
            if (errno != 0 || end == argv[i + 1] || *end != '\0' ||
                !isfinite(seconds) || seconds < 0.001 || seconds > 10) {
                usage(argv[0]);
                return EXIT_FAILURE;
            }
        } else if (strcmp(argv[i], "--rounds") == 0) {
            rounds = strtoul(argv[i + 1], &end, 10);
            if (errno != 0 || end == argv[i + 1] || *end != '\0' ||
                rounds < 1 || rounds > 20) {
                usage(argv[0]);
                return EXIT_FAILURE;
            }
        } else if (strcmp(argv[i], "--pattern") == 0 &&
                   (strcmp(argv[i + 1], "random") == 0 ||
                    strcmp(argv[i + 1], "repeating") == 0)) {
            random_pattern = strcmp(argv[i + 1], "random") == 0;
        } else {
            usage(argv[0]);
            return EXIT_FAILURE;
        }
    }
    printf("SIMD backend: %d (%s); input throughput MiB/s; pattern=%s\n", backend,
           backend == 3 ? "AVX2" : backend == 2 ? "NEON" :
           backend == 1 ? "SSSE3" : "scalar fallback",
           random_pattern ? "random" : "repeating");
    puts("bytes operation scalar simd");
    for (size_t sample = 0; sample < sizeof(sizes) / sizeof(sizes[0]); sample++) {
        const size_t len = sizes[sample];
        uint8_t *src = malloc(len);
        uint8_t *encoded = malloc(len * 2);
        uint8_t *output = malloc(len * 2);
        uint32_t state = UINT32_C(0x6d253fb9);

        if (src == NULL || encoded == NULL || output == NULL) {
            fputs("allocation failed\n", stderr);
            free(src);
            free(encoded);
            free(output);
            return EXIT_FAILURE;
        }
        for (size_t i = 0; i < len; i++) {
            state ^= state << 13;
            state ^= state >> 17;
            state ^= state << 5;
            src[i] = random_pattern ? (uint8_t)state : 0xfb;
        }
        encode(src, len, encoded, 0);
        encode(src, len, output, 1);
        if (memcmp(encoded, output, len * 2) != 0) {
            fputs("SIMD encoding differs from scalar reference\n", stderr);
            return EXIT_FAILURE;
        }
        decode(encoded, len * 2, output, 1);
        if (memcmp(src, output, len) != 0) {
            fputs("SIMD decoding differs from scalar reference\n", stderr);
            return EXIT_FAILURE;
        }
        for (unsigned long round = 0; round < rounds; round++) {
            const double scalar_encode = measure(encode, src, len, output, 0, seconds);
            const double simd_encode = measure(encode, src, len, output, 1, seconds);
            const double scalar_decode = measure(decode, encoded, len * 2, output, 0, seconds);
            const double simd_decode = measure(decode, encoded, len * 2, output, 1, seconds);
            printf("%zu encode %.1f %.1f\n", len, scalar_encode, simd_encode);
            printf("%zu decode %.1f %.1f\n", len * 2, scalar_decode, simd_decode);
        }
        free(output);
        free(encoded);
        free(src);
    }
    return EXIT_SUCCESS;
}
