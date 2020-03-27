-- |
-- Module       : Data.Text.Encoding.Base16
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
module Data.Text.Encoding.Base16
( encodeBase16
, decodeBase16
, decodeBase16With
, decodeBase16Lenient
, isBase16
, isValidBase16
) where


import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Base16.Error (Base16Error(..))
import qualified Data.Text.Encoding as T

-- | Encode a 'Text' value in Base16 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
encodeBase16 :: Text -> Text
encodeBase16 = B16.encodeBase16 . T.encodeUtf8
{-# INLINE encodeBase16 #-}

-- | Decode a padded Base16-encoded lazy 'Text' value, catching unicode
-- exceptions if thrown in the decoding process.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
decodeBase16 :: Text -> Either T.Text Text
decodeBase16 = fmap T.decodeLatin1 . B16.decodeBase16 . T.encodeUtf8
{-# INLINE decodeBase16 #-}

-- | Attempt to decode a lazy 'Text' value as Base16, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.

-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
decodeBase16With
    :: Text
    -> (ByteString -> Either (Base16Error e) Text)
    -> Either (Base16Error e) Text
decodeBase16With t f = case B16.decodeBase16 $ T.encodeUtf8 t of
  Left de -> Left $ DecodeError de
  Right a -> f a
{-# INLINE decodeBase16With #-}

-- | Decode a padded Base16-encoded lazy 'Text' value leniently, using a
-- strategy that never fails, catching unicode exceptions raised in the
-- process of converting to text values.
--
-- N.B.: this is not RFC 4648-compliant.
--
decodeBase16Lenient :: Text -> Text
decodeBase16Lenient = T.decodeLatin1 . B16.decodeBase16Lenient . T.encodeUtf8
{-# INLINE decodeBase16Lenient #-}

-- | Tell whether a 'Text' value is Base16-encoded.
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
isBase16 :: Text -> Bool
isBase16 = B16.isBase16 . T.encodeUtf8
{-# INLINE isBase16 #-}

-- | Tell whether a 'Text' value is a valid Base16 format.
--
-- This will not tell you whether or not this is a correct Base16 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base16 encoded 'Text' value, use 'isBase16'.
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
isValidBase16 :: Text -> Bool
isValidBase16 = B16.isValidBase16 . T.encodeUtf8
{-# INLINE isValidBase16 #-}
