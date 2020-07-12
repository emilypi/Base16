{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Short.Base16
-- Copyright    : (c) 2020 Emily Pillmore
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
, decodeBase16Lenient
, isBase16
, isValidBase16
) where


import Data.ByteString.Short (ShortByteString, fromShort)
import Data.ByteString.Base16.Internal.Head
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short.Unsafe


-- | Encode a 'ShortByteString' value as Base16 'ShortText' with padding.
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
encodeBase16 :: ShortByteString -> ShortText
encodeBase16 = fromShortByteStringUnsafe . decodeBase16ShortLenient_
{-# INLINE encodeBase16 #-}

-- | Encode a 'ShortByteString' value as a Base16 'ShortByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16' "Sun"
-- "53756e"
--
encodeBase16' :: ShortByteString -> ShortByteString
encodeBase16' = encodeBase16Short_
{-# INLINE encodeBase16' #-}

-- | Decode a Base16-encoded 'ShortByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16 "53756e"
-- Right "Sun"
--
-- >>> decodeBase16 "6x"
-- Left "invalid character at offset: 1"
--
decodeBase16 :: ShortByteString -> Either Text ShortByteString
decodeBase16 = decodeBase16Short_
{-# INLINE decodeBase16 #-}

-- | Decode a Base16-encoded 'ShortByteString' value leniently, using a
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

-- | Tell whether a 'ShortByteString' value is base16 encoded.
--
-- === __Examples__:
--
-- >>> isBase16 "666f6"
-- False
--
-- >>> isBase16 "666f"
-- True
--
isBase16 :: ShortByteString -> Bool
isBase16 = B16.isBase16 . fromShort
{-# INLINE isBase16 #-}

-- | Tell whether a 'ShortByteString' value is a valid Base16 format.
--
-- This will not tell you whether or not this is a correct Base16 representation,
-- only that it conforms to the correct alphabet. To check whether it is a true
-- Base16 encoded 'ShortByteString' value, use 'isBase16'.
--
-- === __Examples__:
--
-- >>> isValidBase16 "666f+/6"
-- False
--
-- >>> isValidBase16 "666f6"
-- True
--
isValidBase16 :: ShortByteString -> Bool
isValidBase16 = B16.isValidBase16 . fromShort
{-# INLINE isValidBase16 #-}
