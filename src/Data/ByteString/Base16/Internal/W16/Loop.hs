{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base16.Internal.W16.Loop
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- Encoding loop optimized for 'Word16' architectures
--
module Data.ByteString.Base16.Internal.W16.Loop
( innerLoop
, decodeLoop
) where


import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base16.Internal.Utils
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Word


-- | Hex encoding inner loop optimized for 16-bit architectures
--
innerLoop
    :: Ptr Word16
    -> Ptr Word8
    -> Ptr Word8
    -> IO ()
innerLoop !dptr !sptr !end = go dptr sptr
  where
    lix !a = aix (fromIntegral a .&. 0x0f) alphabet
    {-# INLINE lix #-}

    !alphabet = "0123456789abcdef"#

    go !dst !src
      | src == end = return ()
      | otherwise = do
        !t <- peek src

        let !a = fromIntegral (lix (unsafeShiftR t 4))
            !b = fromIntegral (lix t)

        let !w = a .|. (unsafeShiftL b 8)

        poke dst w

        go (plusPtr dst 2) (plusPtr src 1)
{-# INLINE innerLoop #-}

-- | Hex decoding loop optimized for 16-bit architectures
--
decodeLoop
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> IO (Either Text ByteString)
decodeLoop !dfp !hi !lo !dptr !sptr !end = go dptr sptr 0
  where
    go !dst !src !n
      | src == end = return (Right (PS dfp 0 n))
      | otherwise = do
        !x <- peek @Word8 src
        !y <- peek @Word8 (plusPtr src 1)

        !a <- peekByteOff hi (fromIntegral x)
        !b <- peekByteOff lo (fromIntegral y)

        if a == 0xff || b == 0xff
        then return . Left . T.pack
          $ "invalid character at offset: "
          ++ show (src `minusPtr` sptr)
        else do
          poke dst (a .|. b)
          go (plusPtr dst 1) (plusPtr src 2) (n + 1)
