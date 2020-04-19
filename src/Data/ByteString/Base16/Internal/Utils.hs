{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.ByteString.Base16.Internal.Utils
( aix
, reChunk
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import GHC.Exts
import GHC.Word


-- | Read 'Word8' index off alphabet addr
--
aix :: Word8 -> Addr# -> Word8
aix (W8# i) alpha = W8# (indexWord8OffAddr# alpha (word2Int# i))
{-# INLINE aix #-}

-- | Form a list of chunks, and rechunk the list of bytestrings
-- into length multiples of 2
--
reChunk :: [ByteString] -> [ByteString]
reChunk [] = []
reChunk (c:cs) = case B.length c `divMod` 2 of
    (_, 0) -> c : reChunk cs
    (n, _) -> case B.splitAt (n * 2) c of
      ~(m, q) -> m : cont_ q cs
  where
    cont_ q [] = [q]
    cont_ q (a:as) = case B.splitAt 1 a of
      ~(x, y) -> let q' = B.append q x
        in if B.length q' == 2
          then
            let as' = if B.null y then as else y:as
            in q' : reChunk as'
          else cont_ q' as
