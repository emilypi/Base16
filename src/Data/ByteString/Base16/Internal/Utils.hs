{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.ByteString.Base16.Internal.Utils
( aix
, w32
, w64
, reChunk
, writeNPlainForeignPtrBytes
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.ForeignPtr
import GHC.Word

import System.IO.Unsafe


-- | Read 'Word8' index off alphabet addr
--
aix :: Word8 -> Addr# -> Word8
aix (W8# i) alpha = W8# (indexWord8OffAddr# alpha (word2Int# i))
{-# INLINE aix #-}

w32 :: Word8 -> Word32
w32 = fromIntegral
{-# INLINE w32 #-}

w64 :: Word8 -> Word64
w64 = fromIntegral
{-# INLINE w64 #-}

-- | Allocate and fill @n@ bytes with some data
--
writeNPlainForeignPtrBytes
    :: ( Storable a
       , Storable b
       )
    => Int
    -> [a]
    -> ForeignPtr b
writeNPlainForeignPtrBytes !n as = unsafeDupablePerformIO $ do
    fp <- mallocPlainForeignPtrBytes n
    withForeignPtr fp $ \p -> go p as
    return (castForeignPtr fp)
  where
    go !_ [] = return ()
    go !p (x:xs) = poke p x >> go (plusPtr p 1) xs

-- | Form a list of chunks, and rechunk the list of bytestrings
-- into length multiples of 2
--
reChunk :: [ByteString] -> [ByteString]
reChunk [] = []
reChunk (c:cs) = case B.length c `divMod` 2 of
    (_, 0) -> c : reChunk cs
    (n, _) -> case B.splitAt (n * 2) c of
      (m, q) -> m : cont_ q cs
  where
    cont_ q [] = [q]
    cont_ q (a:as) = case B.splitAt 1 a of
      ~(x, y) -> let q' = B.append q x
        in if B.length q' == 2
          then
            let as' = if B.null y then as else y:as
            in q' : reChunk as'
          else cont_ q' as
