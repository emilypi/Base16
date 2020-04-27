{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Short.Base16
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base16 encoding including
-- unpadded and lenient variants for bytestrings
--
module Data.ByteString.Short.Base16
( encodeBase16
, encodeBase16'
, decodeBase16
, decodeBase16Lenient
, isBase16
, isValidBase16
) where


import Prelude hiding (all, elem)

import Data.ByteString.Short (ShortByteString, fromShort)
import Data.ByteString.Base16.Internal.Head
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)


-- | Encode a 'ShortByteString' value as Base16 'Text' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
encodeBase16 :: ShortByteString -> Text
encodeBase16 = B16.encodeBase16 . fromShort
{-# INLINE encodeBase16 #-}

-- | Encode a 'ShortByteString' value as a Base16 'ShortByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
encodeBase16' :: ShortByteString -> ShortByteString
encodeBase16' = encodeBase16Short_
{-# INLINE encodeBase16' #-}

-- | Decode a Base16-encoded 'ShortByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
decodeBase16 :: ShortByteString -> Either Text ShortByteString
decodeBase16 = decodeBase16Short_
{-# INLINE decodeBase16 #-}

-- | Decode a Base16-encoded 'ShortByteString' value leniently, using a
-- strategy that never fails
--
-- N.B.: this is not RFC 4648-compliant
--
decodeBase16Lenient :: ShortByteString -> ShortByteString
decodeBase16Lenient = decodeBase16ShortLenient_
{-# INLINE decodeBase16Lenient #-}

-- | Tell whether a 'ShortByteString' value is base16 encoded.
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
isBase16 :: ShortByteString -> Bool
isBase16 = B16.isBase16 . fromShort
{-# INLINE isBase16 #-}

-- | Tell whether a 'ShortByteString' value is a valid Base16 format.
--
-- This will not tell you whether or not this is a correct Base16 representation,
-- only that it conforms to the correct alphabet. To check whether it is a true
-- Base16 encoded 'ShortByteString' value, use 'isBase16'.
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
isValidBase16 :: ShortByteString -> Bool
isValidBase16 = B16.isValidBase16 . fromShort
{-# INLINE isValidBase16 #-}
