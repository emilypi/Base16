-- |
-- Module       : Data.ByteString.Short.Base16
-- Copyright    : (c) 2020-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Short.ShortByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base16
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Short.Base16
( encodeBase16
, encodeBase16'
, decodeBase16
, decodeBase16'
, decodeBase16Untyped
, decodeBase16Lenient
, parseBase16
) where


import Data.Base16.Types
import Data.ByteString.Short
import Data.ByteString.Base16.Internal.Head
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import Data.Text.Short
import Data.Text.Short.Unsafe

-- $setup
--
-- >>> import Data.Base16.Types
-- >>> :set -XOverloadedStrings
--

-- | Encode a 'ShortByteString' value as Base16 'ShortText'
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16 "Sun"
-- "53756e"
--
-- @since 0.3.0.0
--
encodeBase16 :: ShortByteString -> Base16 ShortText
encodeBase16 = fmap fromShortByteStringUnsafe . encodeBase16'
{-# INLINE encodeBase16 #-}

-- | Encode a 'ShortByteString' value as a Base16 'ShortByteString'  value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16' "Sun"
-- "53756e"
--
encodeBase16' :: ShortByteString -> Base16 ShortByteString
encodeBase16' = assertBase16 . encodeBase16Short_
{-# INLINE encodeBase16' #-}

-- | Decode a Base16-encoded 'ShortByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16 $ assertBase16 "53756e"
-- "Sun"
--
decodeBase16 :: Base16 ShortByteString -> ShortByteString
decodeBase16 = decodeBase16ShortTyped_
{-# INLINE decodeBase16 #-}

-- | Decode Base16-encoded 'ShortText' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16' $ assertBase16 "53756e"
-- "Sun"
--
decodeBase16' :: Base16 ShortText -> ShortByteString
decodeBase16' = decodeBase16 . fmap toShortByteString
{-# INLINE decodeBase16' #-}

-- | Decode an untyped Base16-encoded 'ByteString' value with error-checking.
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
decodeBase16Untyped :: ShortByteString -> Either Text ShortByteString
decodeBase16Untyped = decodeBase16Short_
{-# INLINE decodeBase16Untyped #-}

-- | Decode an untyped Base16-encoded 'ShortByteString' value leniently, using a
-- strategy that never fails
--
-- N.B.: this is not RFC 4648-compliant
--
--
-- === __Examples__:
--
-- >>> decodeBase16Lenient "53756e"
-- "Sun"
--
-- >>> decodeBase16Lenient "6x6x"
-- "f"
--
decodeBase16Lenient :: ShortByteString -> ShortByteString
decodeBase16Lenient = decodeBase16ShortLenient_
{-# INLINE decodeBase16Lenient #-}

-- | Validate an untyped Base16 value and retain its encoded representation.
--
-- Accepts even-length ASCII hexadecimal input, including upper-case and
-- mixed-case digits. Empty input is valid. The original input is preserved,
-- and errors match 'decodeBase16Untyped'.
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
parseBase16 :: ShortByteString -> Either Text (Base16 ShortByteString)
parseBase16 bs = assertBase16 bs <$ B16.parseBase16 (fromShort bs)
{-# INLINE parseBase16 #-}
