{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ByteString.Base16.Internal.Head
( encodeBase16_
, decodeBase16_
, decodeBase16Lenient_
, encodeBase16Short_
, decodeBase16Short_
, decodeBase16ShortLenient_
) where


#include "MachDeps.h"

import qualified Data.ByteString as BS (empty)
import Data.ByteString.Internal
import qualified Data.ByteString.Short as SBS (empty)
import Data.ByteString.Base16.Internal.Utils
import Data.ByteString.Base16.Internal.W16.Loop
import qualified Data.ByteString.Base16.Internal.W16.ShortLoop as Short
import Data.ByteString.Short.Internal (ShortByteString(..))
import Data.Primitive.ByteArray
import Data.Text (Text)

import Foreign.Ptr
import Foreign.ForeignPtr

import GHC.Exts
import GHC.ForeignPtr

import System.IO.Unsafe


-- | Head of the base16 encoding loop - marshal data, assemble loops
--
encodeBase16_ :: ByteString -> ByteString
encodeBase16_ (PS !sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
      withForeignPtr sfp $ \sptr ->
        innerLoop
          (castPtr dptr)
          (castPtr (plusPtr sptr soff))
          (plusPtr sptr (soff + slen))
  where
    !dlen = 2 * slen

decodeBase16_ :: ByteString -> Either Text ByteString
decodeBase16_ (PS !sfp !soff !slen)
  | slen == 0 = Right BS.empty
  | r /= 0 = Left "invalid bytestring size"
  | otherwise = unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes q
    withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr ->
        decodeLoop
          dfp
          dptr
          (plusPtr sptr soff)
          (plusPtr sptr (soff + slen))
          0
  where
    (!q, !r) = slen `divMod` 2

decodeBase16Lenient_ :: ByteString -> ByteString
decodeBase16Lenient_ (PS !sfp !soff !slen)
  | slen == 0 = BS.empty
  | otherwise = unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes q
    withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr ->
        lenientLoop
          dfp
          dptr
          (plusPtr sptr soff)
          (plusPtr sptr (soff + slen))
          0
  where
    (!q, _) = slen `divMod` 2

-- ---------------------------------------------------------------- --
-- Short encode/decode

encodeBase16Short_ :: ShortByteString -> ShortByteString
encodeBase16Short_ (SBS !ba#) = runShortST $ do
    dst <- newByteArray l'
    Short.innerLoop l dst (MutableByteArray (unsafeCoerce# ba#))
    unsafeFreezeByteArray dst
  where
    !l = I# (sizeofByteArray# ba#)
    !l' = l * 2

decodeBase16Short_ :: ShortByteString -> Either Text ShortByteString
decodeBase16Short_ (SBS !ba#)
    | l == 0 = Right SBS.empty
    | r /= 0 = Left "invalid bytestring size"
    | otherwise = runDecodeST $ do
      dst <- newByteArray q
      Short.decodeLoop l dst (MutableByteArray (unsafeCoerce# ba#))
  where
    !l = I# (sizeofByteArray# ba#)
    (!q, !r) = divMod l 2

decodeBase16ShortLenient_ :: ShortByteString -> ShortByteString
decodeBase16ShortLenient_ (SBS !ba#)
    | l == 0 = SBS.empty
    | otherwise = runShortST $ do
      dst <- newByteArray q
      q' <- Short.lenientLoop l dst (MutableByteArray (unsafeCoerce# ba#))
      !_ <- resizeMutableByteArray dst q'
      unsafeFreezeByteArray dst
  where
    !l = I# (sizeofByteArray# ba#)
    (!q, _) = divMod l 2
