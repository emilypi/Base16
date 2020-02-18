{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base16.Internal.W64.Loop
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- Encoding loop optimized for 'Word64' architectures
--
module Data.ByteString.Base16.Internal.W64.Loop
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


-- | Hex encoding inner loop optimized for 64-bit architectures
--
innerLoop
    :: Ptr Word64
    -> Ptr Word32
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

    tailRound32 !dst !src
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

        tailRound32 (plusPtr dst 4) (plusPtr src 2)

    go !dst !src
      | plusPtr src 7 >= end = tailRound32 (castPtr dst) (castPtr src)
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek src
#else
        !t <- byteSwap32 <$> peek @Word32 src
#endif
        let !a = unsafeShiftR t 28
            !b = unsafeShiftR t 24
            !c = unsafeShiftR t 20
            !d = unsafeShiftR t 16
            !e = unsafeShiftR t 12
            !f = unsafeShiftR t 8
            !g = unsafeShiftR t 4

        let !p = w64 (lix a)
            !q = w64 (lix b)
            !r = w64 (lix c)
            !s = w64 (lix d)
            !w = w64 (lix e)
            !x = w64 (lix f)
            !y = w64 (lix g)
            !z = w64 (lix t)

        let !xx = p
              .|. (unsafeShiftL q 8)
              .|. (unsafeShiftL r 16)
              .|. (unsafeShiftL s 24)

            !yy = w
              .|. (unsafeShiftL x 8)
              .|. (unsafeShiftL y 16)
              .|. (unsafeShiftL z 24)

        let !zz = xx .|. unsafeShiftL yy 32

        poke dst zz

        go (plusPtr dst 8) (plusPtr src 4)
{-# INLINE innerLoop #-}


-- | Hex decoding loop optimized for 64-bit architectures
--
decodeLoop
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word32
  -> Ptr Word64
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

        !a <- peekByteOff @Word8 hi (fromIntegral x)
        !b <- peekByteOff @Word8 lo (fromIntegral y)

        if
          | a == 0xff -> err src
          | b == 0xff -> err (plusPtr src 1)
          | otherwise -> do
            poke dst (a .|. b)
            go (plusPtr dst 1) (plusPtr src 2) (n + 1)

    tailRound32 !dst !src !n
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

    go !dst !src !n
      | plusPtr src 7 >= end = tailRound32 (castPtr dst) (castPtr src) n
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !tt <- peek @Word64 src
#else
        !tt <- byteSwap64 <$> peek @Word64 src
#endif
        let !s = fromIntegral ((unsafeShiftR tt 56) .&. 0xff)
            !t = fromIntegral ((unsafeShiftR tt 48) .&. 0xff)
            !u = fromIntegral ((unsafeShiftR tt 40) .&. 0xff)
            !v = fromIntegral ((unsafeShiftR tt 32) .&. 0xff)
            !w = fromIntegral ((unsafeShiftR tt 24) .&. 0xff)
            !x = fromIntegral ((unsafeShiftR tt 16) .&. 0xff)
            !y = fromIntegral ((unsafeShiftR tt 8) .&. 0xff)
            !z = fromIntegral (tt .&. 0xff)

        !a <- peekByteOff @Word8 hi s
        !b <- peekByteOff @Word8 lo t
        !c <- peekByteOff @Word8 hi u
        !d <- peekByteOff @Word8 lo v
        !e <- peekByteOff @Word8 hi w
        !f <- peekByteOff @Word8 lo x
        !g <- peekByteOff @Word8 hi y
        !h <- peekByteOff @Word8 lo z

        let !zz = fromIntegral (a .|. b)
               .|. (unsafeShiftL (fromIntegral (c .|. d)) 8)
               .|. (unsafeShiftL (fromIntegral (e .|. f)) 16)
               .|. (unsafeShiftL (fromIntegral (g .|. h)) 24)

        if
          | a == 0xff -> err src
          | b == 0xff -> err (plusPtr src 1)
          | c == 0xff -> err (plusPtr src 2)
          | d == 0xff -> err (plusPtr src 3)
          | e == 0xff -> err (plusPtr src 4)
          | f == 0xff -> err (plusPtr src 5)
          | g == 0xff -> err (plusPtr src 6)
          | h == 0xff -> err (plusPtr src 7)
          | otherwise -> do
            poke @Word32 dst zz
            go (plusPtr dst 4) (plusPtr src 8) (n + 4)
