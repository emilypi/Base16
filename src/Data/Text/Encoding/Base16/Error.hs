-- |
-- Module       : Data.Text.Encoding.Base16.Error
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the error types raised (not as exceptions!)
-- in the decoding process.
--
module Data.Text.Encoding.Base16.Error
( Base16Error(..)
) where


import Data.Text (Text)

-- | This data type represents the type of decoding errors of
-- various kinds as they pertain to decoding 'Text' values.
-- Namely, to distinguish between decoding errors from opaque
-- unicode exceptions caught in the unicode decoding process.
--
data Base16Error e
  = DecodeError Text
    -- ^ The error associated with decoding failure
    -- as a result of the Base16 decoding process
  | ConversionError e
    -- ^ The error associated with the decoding failure
    -- as a result of the conversion process `'ByteString' -> 'Text'
  deriving (Eq, Show)
