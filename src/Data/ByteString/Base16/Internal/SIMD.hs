{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module       : Data.ByteString.Base16.Internal.SIMD
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Bounded vector prefixes. The caller retains both buffers with
-- @withForeignPtr@ and finishes through the scalar loops. Invalid decode
-- blocks are left untouched so scalar code preserves exact error offsets.
module Data.ByteString.Base16.Internal.SIMD
( encodePrefix
, decodePrefix
) where

import Data.Word (Word8)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr)

foreign import ccall unsafe "hs_base16_simd_encode"
  c_encodePrefix :: Ptr Word8 -> CSize -> Ptr Word8 -> IO CSize

foreign import ccall unsafe "hs_base16_simd_decode"
  c_decodePrefix :: Ptr Word8 -> CSize -> Ptr Word8 -> IO CSize

-- | Encode complete blocks; return consumed input bytes and write twice as
-- many bytes. The caller must allocate a sufficiently large output buffer.
encodePrefix :: Ptr Word8 -> Int -> Ptr Word8 -> IO Int
encodePrefix src len dst
  | len < 64 = pure 0
  | otherwise = fromIntegral <$> c_encodePrefix src (fromIntegral len) dst
{-# INLINE encodePrefix #-}

-- | Decode complete valid blocks, returning an even input byte count and
-- writing half as many bytes. Small inputs avoid dispatch/foreign-call cost.
decodePrefix :: Ptr Word8 -> Int -> Ptr Word8 -> IO Int
decodePrefix src len dst
  | len < 128 = pure 0
  | otherwise = fromIntegral <$> c_decodePrefix src (fromIntegral len) dst
{-# INLINE decodePrefix #-}
