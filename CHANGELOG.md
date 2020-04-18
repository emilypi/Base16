# Revision history for base16

## 0.2.0.1

* Improved performance. Decode and encode are now 3.5x-5x the existing hex encoding libray performance.

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
