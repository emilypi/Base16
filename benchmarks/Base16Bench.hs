{-# LANGUAGE PackageImports #-}
module Main
( main
)where


import Criterion
import Criterion.Main

import Data.ByteString.Lazy (fromStrict)
import "memory" Data.ByteArray.Encoding as Mem
import Data.ByteString
import "base16" Data.ByteString.Base16 as B16
import "base16-bytestring" Data.ByteString.Base16 as Bos
import Data.ByteString.Random (random)


main :: IO ()
main =
  defaultMain
    [ env bs $ \ ~((bs25,bs100,bs1k,bs10k,bs100k,bs1mm),(bs25L,bs100L,bs1kL,bs10kL,bs100kL,bs1mmL)) ->
      bgroup "encode"
      [ bgroup "25"
        [ bench "memory" $ whnf ctob bs25
        , bench "base16-bytestring" $ whnf Bos.encode bs25
        , bench "base16" $ whnf B16.encodeBase16' bs25
        ]
      , bgroup "100"
        [ bench "memory" $ whnf ctob bs100
        , bench "base16-bytestring" $ whnf Bos.encode bs100
        , bench "base16" $ whnf B16.encodeBase16' bs100
        ]
      , bgroup "1k"
        [ bench "memory" $ whnf ctob bs1k
        , bench "base16-bytestring" $ whnf Bos.encode bs1k
        , bench "base16" $ whnf B16.encodeBase16' bs1k
        ]
      , bgroup "10k"
        [ bench "memory" $ whnf ctob bs10k
        , bench "base16-bytestring" $ whnf Bos.encode bs10k
        , bench "base16" $ whnf B16.encodeBase16' bs10k
        ]
      , bgroup "100k"
        [ bench "memory" $ whnf ctob bs100k
        , bench "base16-bytestring" $ whnf Bos.encode bs100k
        , bench "base16" $ whnf B16.encodeBase16' bs100k
        ]
      , bgroup "1mm"
        [ bench "memory" $ whnf ctob bs1mm
        , bench "base16-bytestring" $ whnf Bos.encode bs1mm
        , bench "base16" $ whnf B16.encodeBase16' bs1mm
        ]
      ]
    , env bs' $ \ ~((bs25,bs100,bs1k,bs10k,bs100k,bs1mm),(bs25L,bs100L,bs1kL,bs10kL,bs100kL,bs1mmL)) ->
      bgroup "decode"
      [ bgroup "25"
        [ bench "memory" $ whnf cfob bs25
        , bench "base16-bytestring" $ whnf Bos.decode bs25
        , bench "base16" $ whnf B16.decodeBase16Lenient bs25
        ]
      , bgroup "100"
        [ bench "memory" $ whnf cfob bs100
        , bench "base16-bytestring" $ whnf Bos.decode bs100
        , bench "base16" $ whnf B16.decodeBase16Lenient bs100
        ]
      , bgroup "1k"
        [ bench "memory" $ whnf cfob bs1k
        , bench "base16-bytestring" $ whnf Bos.decode bs1k
        , bench "base16" $ whnf B16.decodeBase16Lenient bs1k
        ]
      , bgroup "10k"
        [ bench "memory" $ whnf cfob bs10k
        , bench "base16-bytestring" $ whnf Bos.decode bs10k
        , bench "base16" $ whnf B16.decodeBase16Lenient bs10k
        ]
      , bgroup "100k"
        [ bench "memory" $ whnf cfob bs100k
        , bench "base16-bytestring" $ whnf Bos.decode bs100k
        , bench "base16" $ whnf B16.decodeBase16Lenient bs100k
        ]
      , bgroup "1mm"
        [ bench "memory" $ whnf cfob bs1mm
        , bench "base16-bytestring" $ whnf Bos.decode bs1mm
        , bench "base16" $ whnf B16.decodeBase16Lenient bs1mm
        ]
      ]
    ]
  where
    ctob :: ByteString -> ByteString
    ctob = Mem.convertToBase Mem.Base16

    cfob :: ByteString -> Either String ByteString
    cfob = Mem.convertFromBase Mem.Base16

    bs = do
      a <- random 25
      b <- random 100
      c <- random 1000
      d <- random 10000
      e <- random 100000
      f <- random 1000000
      return ((a,b,c,d,e,f),(fromStrict a,fromStrict b,fromStrict c,fromStrict d,fromStrict e,fromStrict f))

    bs' = do
      a <- ctob <$> random 25
      b <- ctob <$> random 100
      c <- ctob <$> random 1000
      d <- ctob <$> random 10000
      e <- ctob <$> random 100000
      f <- ctob <$> random 1000000
      return ((a,b,c,d,e,f),(fromStrict a,fromStrict b,fromStrict c,fromStrict d,fromStrict e,fromStrict f))
