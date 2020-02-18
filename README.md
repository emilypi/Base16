# Base16

[![Build Status](https://travis-ci.com/emilypi/base16.svg?branch=master)](https://travis-ci.com/emilypi/base16)
[![Hackage](https://img.shields.io/hackage/v/base16.svg)](https://hackage.haskell.org/package/base16)

Padded and unpadded base16 and base16hex encoding and decoding for `Text` and `ByteString` values.

For the companion optics and pattern synonyms, see [base16-lens](https://hackage.haskell.org/package/base16-lens).


### Summary

What does this library provide? Here is the summary:

- *Great* encoding performance compared to existing libraries (e.g. `memory`, `base16-bytestring`)
- Better decoding performance compared to existing libraries.
- Support for `Text` encodings and decodings
- Optics for handling more complex structures with Base16 representations via the `base16-lens` package
- Checks for both valid Base16 and correct Base16 and Base16hex encodings

There are no dependencies aside from those bundled with GHC.
