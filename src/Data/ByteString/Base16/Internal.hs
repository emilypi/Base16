{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base16.Internal
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- Internal module defining the encoding and decoding
-- processes and tables.
--
module Data.ByteString.Base16.Internal
( validateBase16
) where


import qualified Data.ByteString as BS
import Data.ByteString.Internal

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

-- -------------------------------------------------------------------------- --
-- Validating Base16

validateBase16 :: ByteString -> ByteString -> Bool
validateBase16 !alphabet (PS fp off l) =
    accursedUnutterablePerformIO $ withForeignPtr fp $ \p ->
      go (plusPtr p off) (plusPtr p (l + off))
  where
    go !p !end
      | p == end = return True
      | otherwise = do
        w <- peek p
        if BS.elem w alphabet
        then go (plusPtr p 1) end
        else return False
{-# INLINE validateBase16 #-}
