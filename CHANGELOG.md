# Revision history for base16

## 2.0

This major release removes two public functions. See
[MIGRATION-2.0.md](MIGRATION-2.0.md) for migration examples.

- Replace `isBase16` and `isValidBase16` with `parseBase16` in all six
  ByteString and Text modules. Parsing checks even length and the mixed-case
  hexadecimal alphabet, then preserves the original encoded representation.
  Boolean checks use `isRight . parseBase16`; odd-length alphabet-only inputs
  previously accepted by `isValidBase16` are now rejected.
- Add an optional manual `simd` flag, off by default, with bundled ARM64 NEON
  and runtime-detected x86-64 AVX2/SSSE3 encoding and strict decoding kernels.
  Unsupported targets and short inputs retain scalar fallbacks; short byte
  array encoding/decoding and lenient decoding remain scalar.
- Preserve scalar decoding errors and input-relative offsets with SIMD.
- Fix lazy typed decoding when hex pairs cross chunk boundaries. Lazy checked
  decoding now materializes the complete input for consistent total-length
  checks and global error offsets. Lazy parsing retains the original chunks.
- Fix [#32](https://github.com/emilypi/Base16/issues/32): lenient Text decoding
  no longer throws on malformed UTF-8. Valid UTF-8 is preserved; invalid bytes
  fall back to their Latin-1 characters. Apply the same policy to lazy Text
  and ShortText, avoiding invalid UTF-8 inside the short-text result.
- Keep the opaque `Base16` type and instances together in `Data.Base16.Types`;
  expose the remaining implementation modules through Cabal.
- Require GHC 9.6 or later, support GHC 9.6 through 9.14, refresh CI, and retire
  the old i386 CI job. Test both SIMD settings from the source distribution.
- Add SIMD boundary/differential tests, native sanitizer and guard-page checks,
  and benchmark coverage around vector and foreign-call boundaries.
- Guard encoded output size calculations against integer overflow.
- Update copyright notices through 2026.

## 1.0

- Revamp api (see: [MIGRATION-1.0.md](MIGRATION-1.0.md) in the source repo)
  - Add new `Base16` opaque wrapper for encoded strings
  - Move `decode*` to `decode*Typed` for untyped bytestring values
  - Make typed decoding the default
  - Optimize loops in the typed case

- Bump lower bound on `bytestring` to `bytestring-0.11`
- Bump lower bound on `text` to `text-2.0`
- Support for all recent GHCs
- Removed support for SafeHaskell

## 0.3.2.1

* Add support for GHC 9.4
* Added `decodeBase16'` to `Data.ByteString.Base16*` (thanks to @shlevy [#20](https://github.com/emilypi/Base16/pull/20))
## 0.3.2.0

* Fix incorrect behavior in `Data.ByteString.Short.Base16#encodeBase16`
* Drop base GHC <8.10

## 0.3.0.1

* Allow for mixed-case lenient decoding + validation
* Bump testing for mixed-case hex

## 0.3.0.0

* API for `decodeBase16With` has changed to require `ByteString` instead of `Text`. This is in alignment with work done on `base64`, which reflects
  the same API. This will be the final design for these types of conversions.
* Test coverage now at 94%
* Add NFData, Exception, and Generic instances for Base16Error + @since annotations for new instances. ([#5](https://github.com/emilypi/Base16/pull/5))
* Doc improvements and add -XTrustworty and -XSafe annotations where needed. ([#5](https://github.com/emilypi/Base16/pull/5))
* Optimized inner loop for short text and bytestrings ([#4](https://github.com/emilypi/Base16/pull/4))
* Changed `encodeBase16` in `ByteString.Short` to produce `ShortText`, instead of `Text`.

## 0.2.1

* Added support for `Text.Short` and `ByteString.Short` values

## 0.2.0.1

* Improved performance. Decode and encode are now 3.5x-5x the next best lib.

## 0.2.0

* Add lenient decoders
* Fix bug in `Text` `decodeBase16` which failed on invalid UTF-8 values as a result of decoding
* Add `decodeBase16With` combinators

## 0.1.3

* Add lazy variants for `Text` and `ByteString` values

## 0.1.2.1 -- 2020-02-17

* Documentation now references correct RFC section

## 0.1.2 -- 2020-02-17

* Unmask loops - now correct.

## 0.1.1 -- 2020-02-17

* Mask `Word32` and `Word64` loops (flaky)

## 0.1.0.0 -- 2020-02-16

* First version. Released on an unsuspecting world.
