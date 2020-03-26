{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ByteString.Base16.Internal.Head
( encodeBase16_
, decodeBase16_
, decodeBase16Lenient_
) where


#include "MachDeps.h"

import Data.ByteString (empty)
import Data.ByteString.Internal
import Data.ByteString.Base16.Internal.Tables
#if WORD_SIZE_IN_BITS == 32
import Data.ByteString.Base16.Internal.W32.Loop
#elif WORD_SIZE_IN_BITS >= 64
import Data.ByteString.Base16.Internal.W64.Loop
#else
import Data.ByteString.Base16.Internal.W16.Loop
#endif
import Data.Text (Text)

import Foreign.Ptr
import Foreign.ForeignPtr

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
  | slen == 0 = Right ""
  | r /= 0 = Left "invalid bytestring size"
  | otherwise = unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes q
    withForeignPtr dfp $ \dptr ->
      withForeignPtr dtableHi $ \hi ->
      withForeignPtr dtableLo $ \lo ->
      withForeignPtr sfp $ \sptr ->
        decodeLoop
          dfp
          hi
          lo
          (castPtr dptr)
          (castPtr (plusPtr sptr soff))
          (plusPtr sptr (soff + slen))
          0
  where
    (!q, !r) = slen `divMod` 2

decodeBase16Lenient_ :: ByteString -> ByteString
decodeBase16Lenient_ (PS !sfp !soff !slen)
  | slen == 0 = empty
  | otherwise = unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes dlen
    withForeignPtr dfp $ \dptr ->
      withForeignPtr dtableHi $ \hi ->
      withForeignPtr dtableLo $ \lo ->
      withForeignPtr sfp $ \sptr ->
        lenientLoop
          dfp
          hi
          lo
          (castPtr dptr)
          (castPtr (plusPtr sptr soff))
          (plusPtr sptr (soff + slen))
          0
  where
    (!q, _) = slen `divMod` 2
    !dlen = q * 2
