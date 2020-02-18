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
-- unpadded and lenient variants
--
module Data.Text.Encoding.Base16
( encodeBase16
, decodeBase16
, isBase16
, isValidBase16
) where


import qualified Data.ByteString.Base16 as B16

import Data.Text (Text)
import qualified Data.Text.Encoding as T

-- | Encode a 'Text' value in Base16 with padding.
--
-- See: <https://tools.ietf.org/html/rfc8688#section-8 RFC-8688 section 8>
--
encodeBase16 :: Text -> Text
encodeBase16 = B16.encodeBase16 . T.encodeUtf8
{-# INLINE encodeBase16 #-}

-- | Decode a padded Base16-encoded 'Text' value
--
-- See: <https://tools.ietf.org/html/rfc8688#section-8 RFC-8688 section 8>
--
decodeBase16 :: Text -> Either Text Text
decodeBase16 = fmap T.decodeUtf8 . B16.decodeBase16 . T.encodeUtf8
{-# INLINE decodeBase16 #-}

-- | Tell whether a 'Text' value is Base16-encoded.
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
isValidBase16 :: Text -> Bool
isValidBase16 = B16.isValidBase16 . T.encodeUtf8
{-# INLINE isValidBase16 #-}
