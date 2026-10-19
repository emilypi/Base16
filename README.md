# Base16

![Build Status](https://github.com/emilypi/base16/workflows/ci/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/base16.svg)](https://hackage.haskell.org/package/base16)

RFC 4648-compliant Base16 encoding and decoding for strict, lazy, and short
ByteString and Text values. Encoding produces lowercase hex; decoding accepts
lowercase, uppercase, and mixed case.

The API tracks encoded values with the `Base16` wrapper. Use `parseBase16` to
validate external input and retain its original representation:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Base16.Types (extractBase16)
import qualified Data.ByteString.Base16 as B16

extractBase16 (B16.encodeBase16' "Sun") -- "53756e"
B16.parseBase16 "53756E"               -- Right "53756E"
B16.decodeBase16Untyped "53756E"       -- Right "Sun"
B16.parseBase16 "abc"                  -- Left "invalid bytestring size"
```

For a Boolean check, use `isRight . parseBase16` from `Data.Either`. Version 2.0
removes `isBase16` and `isValidBase16`; see [MIGRATION-2.0.md](MIGRATION-2.0.md)
for replacement examples, lazy decoding changes, and compiler requirements.
For companion optics and pattern synonyms, see
[base16-lens](https://hackage.haskell.org/package/base16-lens).

Lenient Text decoding ignores non-hex characters and a final unpaired hex
digit. It preserves valid UTF-8 sequences and interprets each invalid UTF-8
byte as Latin-1, so malformed input does not raise Unicode exceptions. This
also applies to lazy Text and ShortText; see the migration guide for details.

## SIMD

The optional, manual `simd` flag enables bundled ARM64 NEON and runtime-detected
x86-64 AVX2/SSSE3 kernels for strict ByteString encoding and decoding, including
the lazy and Text APIs built on those operations. Unsupported targets and
short inputs fall back to Haskell scalar loops. ShortByteString/ShortText encoding and decoding, and lenient decoding, retain
their scalar implementations. Short parsers validate through the strict path.

The flag is off by default. Enable it in your `cabal.project`:

```cabal
package base16
  flags: +simd
```

No external native library is needed. SIMD uses the same alphabet checks and
error offsets as the scalar implementation.

## Development

GHC 9.6 or later is required. Test both build configurations:

```sh
cabal test --flags=-simd
cabal test --flags=simd
cabal bench --flags=-simd --benchmark-options='--iters 1'
cabal bench --flags=simd --benchmark-options='--iters 1'
```

The benchmark suite covers small inputs, vector/call boundaries, and buffers
up to one million bytes. For timings, omit `--iters 1` and use separate build
directories (`--builddir=dist-scalar` and `--builddir=dist-simd`). Compare on the
same machine and compiler; gains depend on input size and CPU.

Native differential tests check exact buffer bounds, alignment, invalid bytes,
and scalar fallback. CI runs them with address/undefined-behavior sanitizers
and guard pages, and builds the Haskell tests, benchmarks, and Haddock from the
source distribution with both flag settings.
