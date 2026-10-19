-- |
-- Module       : Data.Text.Encoding.Base16
-- Copyright    : (c) 2020-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Text'-valued combinators for
-- implementing the RFC 4648 specification of the Base16
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.Text.Encoding.Base16
( encodeBase16
, decodeBase16
, decodeBase16Untyped
, decodeBase16With
, decodeBase16Lenient
, parseBase16
) where


import Data.Base16.Types
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Char (chr)
import Data.Text (Text)
import Data.Text.Encoding.Base16.Error (Base16Error(..))
import qualified Data.Text.Encoding as T

-- $setup
--
-- >>> import Data.Base16.Types
-- >>> :set -XOverloadedStrings
--

-- | Encode a 'Text' value in Base16.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16 "Sun"
-- "53756e"
--
encodeBase16 :: Text -> Base16 Text
encodeBase16 = B16.encodeBase16 . T.encodeUtf8
{-# INLINE encodeBase16 #-}

-- | Decode a Base16-encoded 'Text' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16 $ assertBase16 "53756e"
-- "Sun"
--
decodeBase16 :: Base16 Text -> Text
decodeBase16 = T.decodeUtf8 . B16.decodeBase16 . fmap T.encodeUtf8
{-# INLINE decodeBase16 #-}

-- | Decode an untyped Base16-encoded 'Text' value.
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
decodeBase16Untyped :: Text -> Either Text Text
decodeBase16Untyped = fmap T.decodeUtf8 . B16.decodeBase16Untyped . T.encodeUtf8
{-# INLINE decodeBase16Untyped #-}

-- | Attempt to decode an untyped 'Text' value as Base16, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Example__:
--
-- @
-- 'decodeBase16With' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base16Error' 'UnicodeException') 'Text'
-- @
--
-- @since 0.3.0.0
--
decodeBase16With
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input to decode
    -> Either (Base16Error err) Text
decodeBase16With f t = case B16.decodeBase16Untyped t of
    Left de -> Left $ DecodeError de
    Right a -> first ConversionError (f a)
{-# INLINE decodeBase16With #-}

-- | Decode an untyped Base16-encoded 'Text' value leniently.
--
-- Non-hexadecimal characters and a final unpaired hex digit are ignored.
-- Valid UTF-8 sequences in the decoded bytes are preserved. Each invalid
-- UTF-8 byte is interpreted as its Latin-1 character (U+0000 through U+00FF),
-- so malformed input does not raise Unicode exceptions.
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
-- >>> decodeBase16Lenient "df" == "\223"
-- True
--
-- >>> decodeBase16Lenient "c39f" == "\223"
-- True
--
decodeBase16Lenient :: Text -> Text
decodeBase16Lenient = T.decodeUtf8With (\_ -> fmap (chr . fromIntegral))
  . B16.decodeBase16Lenient
  . T.encodeUtf8
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
parseBase16 :: Text -> Either Text (Base16 Text)
parseBase16 bs = assertBase16 bs <$ B16.parseBase16 (T.encodeUtf8 bs)
{-# INLINE parseBase16 #-}
