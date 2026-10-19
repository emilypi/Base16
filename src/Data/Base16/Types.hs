{-# language ExplicitNamespaces #-}
-- |
-- Module       : Data.Base16.Types
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                sofia-m-a <https://github.com/sofia-m-a>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains the opaque 'Base16' type and combinators for
-- working with Base16-encoded values.
--
module Data.Base16.Types
( type Base16
, assertBase16
, extractBase16
) where


import Control.DeepSeq (NFData(..))
import Data.Functor.Classes (Eq1(..), Ord1(..))

-- | Wrap a value that is or is intended to be Base16-encoded.
--
-- Use the @parseBase16@ function in the corresponding ByteString or Text
-- module to validate external input. Encoding functions also produce this
-- wrapper. 'assertBase16' and the 'Functor', 'Applicative', and 'Monad'
-- instances do not validate their results.
--
newtype Base16 a = Base16 a

instance Eq a => Eq (Base16 a) where
  Base16 a == Base16 b = a == b

instance Eq1 Base16 where
  liftEq f (Base16 a) (Base16 b) = f a b

instance Ord a => Ord (Base16 a) where
  compare (Base16 a) (Base16 b) = compare a b

instance Ord1 Base16 where
  liftCompare f (Base16 a) (Base16 b) = f a b

instance Functor Base16 where
  fmap f (Base16 a) = Base16 (f a)

instance Applicative Base16 where
  pure = Base16
  Base16 f <*> Base16 a = Base16 (f a)

instance Monad Base16 where
  return = pure
  Base16 a >>= k = k a

instance Show a => Show (Base16 a) where
  show (Base16 a) = show a

instance NFData a => NFData (Base16 a) where
  rnf (Base16 a) = rnf a

-- | Assert the provenance of a value.
--
-- For untrusted input, use the @parseBase16@ function in the corresponding
-- ByteString or Text module to validate it first.
--
-- /Warning/: This is a blind assertion that a particular
-- value is base16 encoded in some alphabet. If you are not
-- sure of the provenance of the value, you may experience
-- odd behavior when attempting to decode. Use at your own
-- risk. If I see any issues logged on this project from
-- negligent use of this, Sofia and I will smite you.
--
assertBase16 :: a -> Base16 a
assertBase16 = Base16
{-# inline assertBase16 #-}

-- | Forget that a particular value is Base16-encoded
--
extractBase16 :: Base16 a -> a
extractBase16 (Base16 a) = a
{-# inline extractBase16 #-}
