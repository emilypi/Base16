{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
-- Copyright (c) 2020-2026 Emily Pillmore. BSD-3-Clause; see LICENSE.
module Main (main) where

import Criterion.Main
import Data.Base16.Types (assertBase16, extractBase16)
import qualified Data.ByteString.Short as SBS
import qualified "base16" Data.ByteString.Base16 as B16
import qualified "base16" Data.ByteString.Short.Base16 as S16
import qualified "base16-bytestring" Data.ByteString.Base16 as Reference
import Data.ByteString.Random (random)

main :: IO ()
main = defaultMain [benchSize n | n <- sizes]
  where
    sizes = [0, 1, 15, 16, 31, 32, 63, 64, 65, 127, 128, 129,
             1000, 10000, 100000, 1000000]

benchSize :: Int -> Benchmark
benchSize size = env inputs $ \ ~(raw, encoded, shortRaw, shortEncoded) ->
  bgroup (show size)
    [ bgroup "encode"
      [ bench variant $ nf (extractBase16 . B16.encodeBase16') raw
      , bench "base16-short" $ nf (extractBase16 . S16.encodeBase16') shortRaw
      , bench "base16-bytestring" $ nf Reference.encode raw
      ]
    , bgroup "decode"
      [ bench (variant ++ "-typed") $ nf B16.decodeBase16 (assertBase16 encoded)
      , bench (variant ++ "-checked") $ nf B16.decodeBase16Untyped encoded
      , bench "base16-short" $ nf S16.decodeBase16 (assertBase16 shortEncoded)
      , bench "base16-bytestring" $ nf Reference.decode encoded
      ]
    , bench ("parse/" ++ variant) $ nf B16.parseBase16 encoded
    ]
  where
    inputs = do
      raw <- random (fromIntegral size)
      let encoded = Reference.encode raw
      pure (raw, encoded, SBS.toShort raw, SBS.toShort encoded)

variant :: String
#ifdef BASE16_SIMD
variant = "base16-simd"
#else
variant = "base16-scalar"
#endif
