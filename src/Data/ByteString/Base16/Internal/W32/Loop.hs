{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base16.Internal.W32.Loop
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- Encoding loop optimized for 'Word32' architectures
--
module Data.ByteString.Base16.Internal.W32.Loop
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


-- | Hex encoding inner loop optimized for 32-bit architectures
--
innerLoop
    :: Ptr Word32
    -> Ptr Word16
    -> Ptr Word8
    -> IO ()
innerLoop !dptr !sptr !end = go dptr sptr
  where
    lix !a = aix (fromIntegral a .&. 0x0f) alphabet
    {-# INLINE lix #-}

    !alphabet = "0123456789abcdef"#

    tailRound16 !dst !src
      | src == end = return ()
      | otherwise = do
        !t <- peek @Word8 src

        let !a = fromIntegral (lix (unsafeShiftR t 4))
            !b = fromIntegral (lix t)

        let !w = a .|. (unsafeShiftL b 8)

        poke @Word16 dst w

        tailRound16 (plusPtr dst 2) (plusPtr src 1)

    go !dst !src
      | plusPtr src 3 >= end = tailRound16 (castPtr dst) (castPtr src)
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek src
#else
        !t <- byteSwap16 <$> peek @Word16 src
#endif
        let !a = unsafeShiftR t 12
            !b = unsafeShiftR t 8
            !c = unsafeShiftR t 4

        let !w = w32 (lix a)
            !x = w32 (lix b)
            !y = w32 (lix c)
            !z = w32 (lix t)

        let !xx = w
              .|. (unsafeShiftL x 8)
              .|. (unsafeShiftL y 16)
              .|. (unsafeShiftL z 24)

        poke @Word32 dst xx

        go (plusPtr dst 4) (plusPtr src 2)

-- | Hex decoding loop optimized for 32-bit architectures
--
decodeLoop
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word16
  -> Ptr Word32
  -> Ptr Word8
  -> IO (Either Text ByteString)
decodeLoop !dfp !hi !lo !dptr !sptr !end = go dptr sptr 0
  where
    err !src = return . Left . T.pack
      $ "invalid character at offset: "
      ++ show (src `minusPtr` sptr)

    tailRound16 !dst !src !n
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
          poke @Word8 dst (a .|. b)
          go (plusPtr dst 1) (plusPtr src 2) (n + 1)

    go !dst !src !n
      | plusPtr src 3 >= end = tailRound16 (castPtr dst) (castPtr src) n
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek @Word32 src
#else
        !t <- byteSwap32 <$> peek @Word32 src
#endif
        let !w = fromIntegral ((unsafeShiftR t 24) .&. 0xff)
            !x = fromIntegral ((unsafeShiftR t 16) .&. 0xff)
            !y = fromIntegral ((unsafeShiftR t 8) .&. 0xff)
            !z = (fromIntegral (t .&. 0xff))

        !a <- peekByteOff @Word8 hi w
        !b <- peekByteOff @Word8 lo x
        !c <- peekByteOff @Word8 hi y
        !d <- peekByteOff @Word8 lo z

        let !zz = fromIntegral (a .|. b)
               .|. (unsafeShiftL (fromIntegral (c .|. d)) 8)
        if
          | a == 0xff -> err src
          | b == 0xff -> err (plusPtr src 1)
          | c == 0xff -> err (plusPtr src 2)
          | d == 0xff -> err (plusPtr src 3)
          | otherwise -> do
            poke @Word16 dst zz
            go (plusPtr dst 2) (plusPtr src 4) (n + 2)
