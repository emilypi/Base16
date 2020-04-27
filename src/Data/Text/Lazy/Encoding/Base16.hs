-- |
-- Module       : Data.Text.Encoding.Base16.Lazy
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base16 encoding including
-- unpadded and lenient variants for lazy textual values
--
module Data.Text.Lazy.Encoding.Base16
( encodeBase16
, decodeBase16
, decodeBase16With
, decodeBase16Lenient
, isBase16
, isValidBase16
) where


import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Base16 as B16L
import qualified Data.Text as T
import Data.Text.Encoding.Base16.Error (Base16Error(..))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TL

-- | Encode a lazy 'Text' value in Base16 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
encodeBase16 :: Text -> Text
encodeBase16 = B16L.encodeBase16 . TL.encodeUtf8
{-# INLINE encodeBase16 #-}

-- | Decode a Base16-encoded lazy 'Text' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
decodeBase16 :: Text -> Either T.Text Text
decodeBase16 = fmap TL.decodeLatin1 . B16L.decodeBase16 . TL.encodeUtf8
{-# INLINE decodeBase16 #-}

-- | Attempt to decode a lazy 'Text' value as Base16, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- Example:
--
-- @
-- 'decodeBase16With' 'TL.decodeUtf8''
--   :: 'Text' -> 'Either' ('Base16Error' 'UnicodeException') 'Text'
-- @
--
decodeBase16With
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> Text
      -- ^ Input text to decode
    -> Either (Base16Error err) Text
decodeBase16With f t = case B16L.decodeBase16 $ TL.encodeUtf8 t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase16With #-}

-- | Decode a Base16-encoded lazy 'Text' value leniently, using a
-- strategy that never fails.
--
-- /Warning/: in the conversion to unicode text, exceptions may be thrown.
-- Please use 'decodeBase16'' if you are unsure if you are working with
-- base16-encoded values, or if you expect garbage.
--
-- N.B.: this is not RFC 4648-compliant. It may give you garbage if you're not careful!
--
decodeBase16Lenient :: Text -> Text
decodeBase16Lenient = TL.decodeLatin1 . B16L.decodeBase16Lenient . TL.encodeUtf8
{-# INLINE decodeBase16Lenient #-}

-- | Tell whether a lazy 'Text' value is Base16-encoded.
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
isBase16 = B16L.isBase16 . TL.encodeUtf8
{-# INLINE isBase16 #-}

-- | Tell whether a lazy 'Text' value is a valid Base16 format.
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
isValidBase16 = B16L.isValidBase16 . TL.encodeUtf8
{-# INLINE isValidBase16 #-}
