# Revision history for base16

## 0.3.1.0

* Add NFData, Exception, and Generic instances for Base16Error + @since annotations for new instances. ([#5](https://github.com/emilypi/Base16/pull/5))
* Doc improvements and add -XTrustworty and -XSafe annotations where needed. ([#5](https://github.com/emilypi/Base16/pull/5))
* Optimized inner loop for short text and bytestrings ([#4](https://github.com/emilypi/Base16/pull/4))

## 0.3.0.0

* Changed `encodeBase16` in `ByteString.Short` to produce `ShortText`, instead of `Text`.
* Optimized `*.Short` variants to make use of custom `ByteArray#`-backed loops

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
