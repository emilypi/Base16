{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module       : Data.ByteString.Base16.Internal.SIMD
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Bounded vector prefixes for foreign pointers and byte arrays. Pointer
-- callers retain both buffers with @withForeignPtr@. Byte-array callers pass
-- the arrays directly to unsafe FFI calls, during which GHC cannot run GC;
-- neither pinning nor copying is needed. The kernels retain no pointers and
-- make no Haskell callbacks. Invalid decode blocks are left untouched so
-- scalar tails preserve exact error offsets.
module Data.ByteString.Base16.Internal.SIMD
( encodePrefix
, decodePrefix
, encodeByteArrayPrefix
, decodeByteArrayPrefix
) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Primitive.ByteArray (ByteArray(..), MutableByteArray(..))
import Data.Word (Word8)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr)
import GHC.Exts (ByteArray#, MutableByteArray#)

foreign import ccall unsafe "hs_base16_simd_encode"
  c_encodePrefix :: Ptr Word8 -> CSize -> Ptr Word8 -> IO CSize

foreign import ccall unsafe "hs_base16_simd_decode"
  c_decodePrefix :: Ptr Word8 -> CSize -> Ptr Word8 -> IO CSize

foreign import ccall unsafe "hs_base16_simd_encode"
  c_encodeByteArrayPrefix :: ByteArray# -> CSize -> MutableByteArray# s -> IO CSize

foreign import ccall unsafe "hs_base16_simd_decode"
  c_decodeByteArrayPrefix :: ByteArray# -> CSize -> MutableByteArray# s -> IO CSize

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

-- | Encode a prefix of an immutable byte array into a distinct mutable
-- destination with room for twice the input length. Return consumed bytes.
-- The foreign call only reads the source and writes the destination, so it
-- can be embedded in ST without letting mutable state escape.
encodeByteArrayPrefix :: ByteArray -> Int -> MutableByteArray s -> ST s Int
encodeByteArrayPrefix (ByteArray src) len (MutableByteArray dst)
  | len < 64 = pure 0
  | otherwise = unsafeIOToST $
      fromIntegral <$> c_encodeByteArrayPrefix src (fromIntegral len) dst
{-# INLINE encodeByteArrayPrefix #-}

-- | Decode complete valid blocks into a distinct mutable destination with
-- room for half the input length. Return an even count of consumed bytes.
decodeByteArrayPrefix :: ByteArray -> Int -> MutableByteArray s -> ST s Int
decodeByteArrayPrefix (ByteArray src) len (MutableByteArray dst)
  | len < 128 = pure 0
  | otherwise = unsafeIOToST $
      fromIntegral <$> c_decodeByteArrayPrefix src (fromIntegral len) dst
{-# INLINE decodeByteArrayPrefix #-}
