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

data MegaFunctor f a where
  Pure :: a -> MegaFunctor f a
  Ap :: f a -> MegaFunctor f (a -> b) -> MegaFunctor f b
  Select :: MegaFunctor f (Either a b) -> f (a -> b) -> MegaFunctor f b
  Join :: MegaFunctor (MegaFunctor f) a -> MegaFunctor f a

instance Functor f => Functor (MegaFunctor f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Ap x y) = Ap x ((f .) <$> y)
  fmap f (Select x y) = Select (fmap f <$> x) (fmap f <$> y)
  fmap f (Bind x y) = Bind x (fmap f <$> y)

instance Functor f => Applicative (MegaFunctor f) where
  pure = Pure
  Pure f <*> y = fmap f y
  Ap x y <*> z = Ap x (flip <$> y <*> z)
  f@(Select _ _) <*> x = select (Left <$> f) ((&) <$> x)
  x@(Bind _ _) <*> y = do
    x1 <- x
    x1 <$> y

instance Functor f => Selective (MegaFunctor f) where
  select x (Pure y) = either y id <$> x -- Generalised identity
  select x y@(Ap _ _) = (\e f -> either f id e) <$> x <*> y
  select x (Select y z) = Select (select (f <$> x) (g <$> y)) (h <$> z) -- Associativity
    where
      f x = Right <$> x
      g y a = bimap (, a) ($a) y
      h = uncurry
  select x y@(Bind _ _) = x >>= \case
    Left a -> ($a) <$> y
    Right b -> pure b

instance Functor f => Monad (MegaFunctor f) where
  return = pure
