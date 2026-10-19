# Migration guide for 2.0

Version 2.0 replaces `isBase16` and `isValidBase16` with `parseBase16` in all
six ByteString and Text modules. The existing strict ByteString parser keeps
its signature; the other modules now provide the corresponding parser:

```haskell
parseBase16 :: a -> Either Text (Base16 a)
```

Here `Text` in `Either` is strict `Data.Text.Text`, including in the lazy and
short modules. The result retains the original encoded value, including its
case and lazy chunk boundaries.

## Replacing Boolean checks

```haskell
import Data.Either (isRight)
import qualified Data.ByteString.Base16 as B16

-- Previously: B16.isBase16 input
isRight (B16.parseBase16 input)
```

The parser accepts empty input and even-length ASCII hexadecimal strings in
lowercase, uppercase, or mixed case. It rejects odd lengths before checking
characters. An odd-length string such as `"abc"` was accepted by
`isValidBase16`, which checked only the alphabet; `parseBase16` rejects it with
`Left "invalid bytestring size"`. For even-length inputs, invalid characters
produce `Left "invalid character at offset: N"`, using a zero-based byte offset
from the start of the complete input (UTF-8 bytes for Text inputs).

Both predicates are removed in this major release, without a deprecation
period. If an application specifically needs an alphabet-only check, define
that separate policy in the application.

## Validate once, then decode

```haskell
case B16.parseBase16 input of
  Left err      -> handleError err
  Right encoded -> useBytes (B16.decodeBase16 encoded)
```

Use `decodeBase16Untyped` when you want validation and decoded bytes together.
`parseBase16` currently validates via the checked decoder and discards its
output, so parsing followed by typed decoding processes the input twice.
`assertBase16` remains an unchecked assertion for values with known provenance.

Text parsers validate the encoded hex, not the character encoding of the
decoded payload. For example, `"ff"` parses successfully even though byte
`0xff` is not valid UTF-8. Use the ByteString decoding functions or
`decodeBase16With` with a checked text conversion when payloads may be binary.

Lazy typed decoding now handles hex pairs split across chunks. Lazy checked
decoding validates the complete input, giving total-length checks and global
error offsets; it materializes the input as a strict ByteString. Parsing
preserves the original chunks despite this validation step.

## Lenient text decoding

`decodeBase16Lenient` in all three Text modules now handles invalid UTF-8
without throwing or constructing invalid ShortText. After skipping non-hex
characters and dropping a final unpaired hex digit, it decodes valid UTF-8
sequences normally and maps each invalid byte to its Latin-1 character.

This fixes [#32](https://github.com/emilypi/Base16/issues/32): decoding
`"0014df1047454e53"` again yields `"\NUL\DC4ß\DLEGENS"`. Both `"df"` (a lone
Latin-1 byte) and `"c39f"` (valid UTF-8) produce `"ß"`. This is a recovery
policy, not automatic encoding detection: valid UTF-8 takes precedence even
inside otherwise malformed input. Lazy decoding handles sequences across
chunk boundaries and remains streaming.

Typed and checked decoders keep their existing UTF-8 behavior. For explicit
encoding control, decode to bytes and choose a conversion, or use
`decodeBase16With` with a checked conversion.

## Optional SIMD acceleration

The manual `simd` flag is disabled by default. To enable it in `cabal.project`:

```cabal
package base16
  flags: +simd
```

For this repository, use `cabal test --flags=simd`. The bundled C kernels use
ARM64 NEON or runtime-detected x86-64 AVX2/SSSE3. Unsupported targets use the
Haskell scalar implementation. No external SIMD library or global CPU-specific
compiler flags are required. Strict ByteString and ShortByteString encoding,
checked decoding, and typed decoding use SIMD where applicable, as do the
lazy/Text APIs built on them. ShortByteString and ShortText use direct
byte-array bindings without pinning or copying through strict ByteString;
their parsers validate through the short decoder. Inputs below the SIMD
thresholds and lenient decoding retain scalar paths.

## Compiler and module support

Like base64 2.0, this release requires GHC 9.6 or later (`base >= 4.18`),
`bytestring >= 0.11`, and `text >= 2.0`. CI covers GHC 9.6 through 9.14 with
SIMD enabled and disabled; the old i386 CI job has been retired.

The `Base16` definition and instances now live together in `Data.Base16.Types`.
Its constructor remains hidden. The remaining implementation modules are
exposed for advanced use, with their `Internal` names marking implementation
details; `Data.ByteString.Base16.Internal.SIMD` is available only with `simd`.
