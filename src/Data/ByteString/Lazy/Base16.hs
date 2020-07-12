{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Lazy.Base16
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Lazy.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base16
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Lazy.Base16
( encodeBase16
, encodeBase16'
, decodeBase16
, decodeBase16Lenient
, isBase16
, isValidBase16
) where



import Prelude hiding (all, elem)

import qualified Data.ByteString as B
import Data.ByteString.Lazy (all, elem, fromChunks, toChunks)
import Data.ByteString.Lazy.Internal (ByteString(..))
import qualified Data.ByteString.Base16.Internal.Head as B16
import Data.ByteString.Base16.Internal.Utils (reChunk)
import Data.Either
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL


-- | Encode a lazy 'ByteString' value as Base16 'Text' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16 "Sun"
-- "53756e"
--
encodeBase16 :: ByteString -> Text
encodeBase16 = TL.decodeUtf8 . encodeBase16'
{-# INLINE encodeBase16 #-}

-- | Encode a lazy 'ByteString' value as a Base16 'ByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16' "Sun"
-- "53756e"
--
encodeBase16' :: ByteString -> ByteString
encodeBase16' Empty = Empty
encodeBase16' (Chunk b bs) = Chunk (B16.encodeBase16_ b) (encodeBase16' bs)
{-# INLINE encodeBase16' #-}

-- | Decode a padded Base16-encoded lazy 'ByteString' value.
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
decodeBase16 :: ByteString -> Either T.Text ByteString
decodeBase16 Empty = Right Empty
decodeBase16 (Chunk b bs) = Chunk <$> B16.decodeBase16_ b <*> decodeBase16 bs
{-# INLINE decodeBase16 #-}

-- | Decode a Base16-encoded 'ByteString' value leniently, using a
-- strategy that never fails
--
-- N.B.: this is not RFC 4648-compliant. It may give you garbage if you're not careful!
--
-- === __Examples__:
--
-- >>> decodeBase16Lenient "53756e"
-- "Sun"
--
-- >>> decodeBase16Lenient "6x6x"
-- "f"
--
decodeBase16Lenient :: ByteString -> ByteString
decodeBase16Lenient = fromChunks
  . fmap B16.decodeBase16Lenient_
  . reChunk
  . fmap (B.filter (flip elem "0123456789abcdef"))
  . toChunks
{-# INLINE decodeBase16Lenient #-}

-- | Tell whether a lazy 'ByteString' value is base16 encoded.
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

-- | Tell whether a lazy 'ByteString' value is a valid Base16 format.
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
