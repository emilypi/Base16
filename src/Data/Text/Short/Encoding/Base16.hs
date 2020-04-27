-- |
-- Module       : Data.Text.Short.Encoding.Base16
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base16 encoding including
-- unpadded and lenient variants for text values
--
module Data.Text.Short.Encoding.Base16
( encodeBase16
, decodeBase16
, decodeBase16With
, decodeBase16Lenient
, isBase16
, isValidBase16
) where


import Data.Bifunctor (first)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short.Base16 as BS16
import Data.Text (Text)
import Data.Text.Encoding.Base16.Error
import Data.Text.Short
import Data.Text.Short.Unsafe


-- | Encode a 'ShortText' value in Base16 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
encodeBase16 :: ShortText -> ShortText
encodeBase16 = fromShortByteStringUnsafe
  . BS16.encodeBase16'
  . toShortByteString
{-# INLINE encodeBase16 #-}

-- | Decode a Base16-encoded lazy 'ShortText' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
decodeBase16 :: ShortText -> Either Text ShortText
decodeBase16 =  fmap fromShortByteStringUnsafe
  . BS16.decodeBase16
  . toShortByteString
{-# INLINE decodeBase16 #-}

-- | Attempt to decode a lazy 'ShortText' value as Base16, converting from
-- 'ByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- Example:
--
-- @
-- 'decodeBase16With' 'T.decodeUtf8''
--   :: 'ShortText' -> 'Either' ('Base16Error' 'UnicodeException') 'ShortText'
-- @
--
decodeBase16With
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortText
      -- ^ Input text to decode
    -> Either (Base16Error err) ShortText
decodeBase16With f t = case BS16.decodeBase16 (toShortByteString t) of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase16With #-}

-- | Decode a Base16-encoded lazy 'ShortText' value leniently, using a
-- strategy that never fails, catching unicode exceptions raised in the
-- process of converting to text values.
--
-- N.B.: this is not RFC 4648-compliant.
--
decodeBase16Lenient :: ShortText -> ShortText
decodeBase16Lenient = fromShortByteStringUnsafe
  . BS16.decodeBase16Lenient
  . toShortByteString
{-# INLINE decodeBase16Lenient #-}

-- | Tell whether a 'ShortText' value is Base16-encoded.
--
-- Examples:
--
-- This example will fail. It conforms to the alphabet, but
-- is not valid because it has an incorrect (odd) length.
--
-- >>> isBase16 "666f6"
-- False
--
-- This example will succeed because it satisfies the alphabet
-- and is considered "valid" (i.e. of the correct size and shape).
--
-- >>> isBase16 "666f"
-- True
--
isBase16 :: ShortText -> Bool
isBase16 = B16.isBase16 . toByteString
{-# INLINE isBase16 #-}

-- | Tell whether a 'ShortText' value is a valid Base16 format.
--
-- This will not tell you whether or not this is a correct Base16 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base16 encoded 'ShortText' value, use 'isBase16'.
--
-- Examples:
--
-- This example will fail because it does not conform to the Hex
-- alphabet.
--
-- >>> isValidBase16 "666f+/6"
-- False
--
-- This example will succeed because it satisfies the alphabet
-- and is considered "valid" (i.e. of the correct size and shape), but
-- is not correct base16 because it is the wrong shape.
--
-- >>> isValidBase16 "666f6"
-- True
--
isValidBase16 :: ShortText -> Bool
isValidBase16 = B16.isValidBase16 . toByteString
{-# INLINE isValidBase16 #-}
