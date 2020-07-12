{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Base16
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: stable
-- Portability	: non-portable
--
-- This module contains 'Data.ByteString.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base16
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Base16
( encodeBase16
, encodeBase16'
, decodeBase16
, decodeBase16Lenient
, isBase16
, isValidBase16
) where


import Prelude hiding (all, elem)

import Data.ByteString (ByteString, all, elem)
import Data.ByteString.Base16.Internal.Head
import Data.Either
import Data.Text (Text)
import qualified Data.Text.Encoding as T


-- | Encode a 'ByteString' value as Base16 'Text' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16 "Sun"
-- "53756e"
--
encodeBase16 :: ByteString -> Text
encodeBase16 = T.decodeUtf8 . encodeBase16'
{-# INLINE encodeBase16 #-}

-- | Encode a 'ByteString' value as a Base16 'ByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16' "Sun"
-- "53756e"
--
encodeBase16' :: ByteString -> ByteString
encodeBase16' = encodeBase16_
{-# INLINE encodeBase16' #-}

-- | Decode a Base16-encoded 'ByteString' value.
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
decodeBase16 :: ByteString -> Either Text ByteString
decodeBase16 = decodeBase16_
{-# INLINE decodeBase16 #-}

-- | Decode a Base16-encoded 'ByteString' value leniently, using a
-- strategy that never fails
--
-- N.B.: this is not RFC 4648-compliant
--
-- === __Examples__:
--
-- >>> decodeBase16Lenient "53756e"
-- "Sun"
--
-- >>> decodeBase16 "6x6x"
-- "f"
--
decodeBase16Lenient :: ByteString -> ByteString
decodeBase16Lenient = decodeBase16Lenient_
{-# INLINE decodeBase16Lenient #-}

-- | Tell whether a 'ByteString' value is base16 encoded.
--
-- === __Examples__:
--
-- >>> isBase16 "666f6"
-- False
--
-- >>> isBase16 "666f"
-- True
--
isBase16 :: ByteString -> Bool
isBase16 bs = isValidBase16 bs && isRight (decodeBase16 bs)
{-# INLINE isBase16 #-}

-- | Tell whether a 'ByteString' value is a valid Base16 format.
--
-- This will not tell you whether or not this is a correct Base16 representation,
-- only that it conforms to the correct alphabet. To check whether it is a true
-- Base16 encoded 'ByteString' value, use 'isBase16'.
--
-- === __Examples__:
--
-- >>> isValidBase16 "666f+/6"
-- False
--
-- >>> isValidBase16 "666f6"
-- True
--
isValidBase16 :: ByteString -> Bool
isValidBase16 = all (flip elem "0123456789abcdef")
{-# INLINE isValidBase16 #-}
