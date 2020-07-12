{-# LANGUAGE Safe #-}
-- |
-- Module       : Data.Text.Encoding.Base16
-- Copyright    : (c) 2020 Emily Pillmore
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
, decodeBase16With
, decodeBase16Lenient
, isBase16
, isValidBase16
) where


import Data.Bifunctor (first)
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
-- === __Examples__:
--
-- >>> encodeBase16 "Sun"
-- "53756e"
--
encodeBase16 :: Text -> Text
encodeBase16 = B16.encodeBase16 . T.encodeUtf8
{-# INLINE encodeBase16 #-}

-- | Decode a Base16-encoded lazy 'Text' value.
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
decodeBase16 :: Text -> Either T.Text Text
decodeBase16 = fmap T.decodeLatin1 . B16.decodeBase16 . T.encodeUtf8
{-# INLINE decodeBase16 #-}

-- | Attempt to decode a lazy 'Text' value as Base16, converting from
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
-- @since 0.4.0.0
--
decodeBase16With
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input to decode
    -> Either (Base16Error err) Text
decodeBase16With f t = case B16.decodeBase16 t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase16With #-}

-- | Decode a Base16-encoded lazy 'Text' value leniently, using a
-- strategy that never fails, catching unicode exceptions raised in the
-- process of converting to text values.
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
decodeBase16Lenient :: Text -> Text
decodeBase16Lenient = T.decodeLatin1 . B16.decodeBase16Lenient . T.encodeUtf8
{-# INLINE decodeBase16Lenient #-}

-- | Tell whether a 'Text' value is Base16-encoded.
--
-- === __Examples__:
--
-- >>> isBase16 "666f6"
-- False
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
-- === __Examples__:
--
-- >>> isValidBase16 "666f+/6"
-- False
--
-- >>> isValidBase16 "666f6"
-- True
--
isValidBase16 :: Text -> Bool
isValidBase16 = B16.isValidBase16 . T.encodeUtf8
{-# INLINE isValidBase16 #-}
