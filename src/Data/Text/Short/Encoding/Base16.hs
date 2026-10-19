-- |
-- Module       : Data.Text.Short.Encoding.Base16
-- Copyright    : (c) 2020-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Short.ShortText'-valued combinators for
-- implementing the RFC 4648 specification of the Base16
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.Text.Short.Encoding.Base16
( encodeBase16
, decodeBase16
, decodeBase16Untyped
, decodeBase16With
, decodeBase16Lenient
, parseBase16
) where


import Data.Base16.Types
import Data.Bifunctor (first)
import Data.ByteString.Short
import qualified Data.ByteString.Short.Base16 as BS16
import Data.Text (Text)
import qualified Data.Text.Encoding.Base16 as T16
import Data.Text.Encoding.Base16.Error
import Data.Text.Short
import Data.Text.Short.Unsafe

-- $setup
--
-- >>> import Data.Base16.Types
-- >>> :set -XOverloadedStrings
--

-- | Encode a 'ShortText' value in Base16.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16 "Sun"
-- "53756e"
--
encodeBase16 :: ShortText -> Base16 ShortText
encodeBase16 = BS16.encodeBase16 . toShortByteString
{-# INLINE encodeBase16 #-}


-- | Decode a Base16-encoded 'ShortText' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16 $ assertBase16 "53756e"
-- "Sun"
--
decodeBase16 :: Base16 ShortText -> ShortText
decodeBase16 =  fromShortByteStringUnsafe
  . BS16.decodeBase16
  . fmap toShortByteString
{-# INLINE decodeBase16 #-}

-- | Decode an untyped Base16-encoded 'ShortText' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16Untyped "53756e"
-- Right "Sun"
--
-- >>> decodeBase16Untyped "6x"
-- Left "invalid character at offset: 1"
--
decodeBase16Untyped :: ShortText -> Either Text ShortText
decodeBase16Untyped =  fmap fromShortByteStringUnsafe
  . BS16.decodeBase16Untyped
  . toShortByteString
{-# INLINE decodeBase16Untyped #-}

-- | Attempt to decode an untyped 'ShortText' value as Base16, converting from
-- 'ByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Example__:
--
-- @
-- 'decodeBase16With' (fmap 'fromText' . 'Data.Text.Encoding.decodeUtf8'' . 'fromShort')
--   :: 'ShortByteString' -> 'Either' ('Base16Error' 'UnicodeException') 'ShortText'
-- @
--
-- @since 0.3.0.0
--
decodeBase16With
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input to decode
    -> Either (Base16Error err) ShortText
decodeBase16With f t = case BS16.decodeBase16Untyped t of
    Left de -> Left $ DecodeError de
    Right a -> first ConversionError $ f a
{-# INLINE decodeBase16With #-}

-- | Decode an untyped Base16-encoded 'ShortText' value leniently.
--
-- Non-hexadecimal characters and a final unpaired hex digit are ignored.
-- Valid UTF-8 sequences are preserved. Each invalid UTF-8 byte is interpreted
-- as its Latin-1 character (U+0000 through U+00FF), ensuring the result is
-- valid 'ShortText' without raising Unicode exceptions.
--
-- N.B.: this is not RFC 4648-compliant.
--
-- === __Examples__:
--
-- >>> decodeBase16Lenient "53756e"
-- "Sun"
--
-- >>> decodeBase16Lenient "6x6x"
-- "f"
--
decodeBase16Lenient :: ShortText -> ShortText
decodeBase16Lenient = fromText . T16.decodeBase16Lenient . toText
{-# INLINE decodeBase16Lenient #-}

-- | Validate an untyped Base16 value and retain its encoded representation.
--
-- Accepts even-length ASCII hexadecimal input, including upper-case and
-- mixed-case digits. Empty input is valid. The original input is preserved,
-- and errors match 'decodeBase16Untyped'.
--
-- This validates the Base16 encoding, not the UTF-8 validity of its decoded
-- payload. Use 'decodeBase16With' with a checked conversion for text decoding.
--
-- === __Examples__:
--
-- >>> parseBase16 "666f6"
-- Left "invalid bytestring size"
--
-- >>> parseBase16 "666F"
-- Right "666F"
--
-- >>> fmap decodeBase16 (parseBase16 "666F")
-- Right "fo"
--
-- >>> parseBase16 "6x"
-- Left "invalid character at offset: 1"
--
-- @since 2.0
--
parseBase16 :: ShortText -> Either Text (Base16 ShortText)
parseBase16 bs = assertBase16 bs <$ BS16.parseBase16 (toShortByteString bs)
{-# INLINE parseBase16 #-}
