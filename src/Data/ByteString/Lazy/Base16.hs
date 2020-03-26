{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Base16.Lazy
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base16 encoding including
-- unpadded and lenient variants for lazy bytestrings
--
module Data.ByteString.Lazy.Base16
( encodeBase16
, encodeBase16'
, decodeBase16
, isBase16
, isValidBase16
) where


import Prelude hiding (all, elem)

import Data.ByteString.Lazy (all, elem)
import Data.ByteString.Lazy.Internal (ByteString(..))
import qualified Data.ByteString.Base16.Internal.Head as B16
import Data.Either
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TL


-- | Encode a lazy 'ByteString' value as Base16 'Text' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
encodeBase16 :: ByteString -> Text
encodeBase16 = TL.decodeUtf8 . encodeBase16'
{-# INLINE encodeBase16 #-}

-- | Encode a lazy 'ByteString' value as a Base16 'ByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
encodeBase16' :: ByteString -> ByteString
encodeBase16' Empty = Empty
encodeBase16' (Chunk b bs) = Chunk (B16.encodeBase16_ b) (encodeBase16' bs)
{-# INLINE encodeBase16' #-}

-- | Decode a padded Base16-encoded lazy 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
decodeBase16 :: ByteString -> Either T.Text ByteString
decodeBase16 Empty = Right Empty
decodeBase16 (Chunk b bs) = Chunk <$> B16.decodeBase16_ b <*> decodeBase16 bs
{-# INLINE decodeBase16 #-}

-- | Tell whether a lazy 'ByteString' value is base16 encoded.
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
isBase16 :: ByteString -> Bool
isBase16 bs = isValidBase16 bs && isRight (decodeBase16 bs)
{-# INLINE isBase16 #-}

-- | Tell whether a lazy 'ByteString' value is a valid Base16 format.
--
-- This will not tell you whether or not this is a correct Base16 representation,
-- only that it conforms to the correct alphabet. To check whether it is a true
-- Base16 encoded 'ByteString' value, use 'isBase16'.
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
isValidBase16 :: ByteString -> Bool
isValidBase16 = all (flip elem "0123456789abcdef")
{-# INLINE isValidBase16 #-}
