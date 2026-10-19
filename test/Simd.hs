{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module       : Simd
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- The same public API tests exercise scalar and SIMD builds. Expected bytes
-- come from an independent codec, so matching encoder and decoder mistakes
-- cannot cancel out as they could in roundtrip-only tests.
module Simd (tests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_, forever)
import Data.Base16.Types (assertBase16, extractBase16)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import qualified "base16" Data.ByteString.Base16 as B16
import qualified "base16" Data.ByteString.Short.Base16 as S16
import qualified "base16-bytestring" Data.ByteString.Base16 as Bos
import Data.Char (toUpper)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Short as TS
import Data.Word (Word8)
import GHC.Exts (isByteArrayPinned#, isTrue#)
import Internal (Harness(..), b16, lb16, sb16, t16, tl16, ts16)
import System.Mem (performGC)
import Test.QuickCheck (arbitrary, choose, conjoin, forAll, vectorOf, (===))
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (QuickCheckTests(..), testProperty)

tests :: TestTree
tests = testGroup "SIMD compatibility"
  [ testCase "vector and dispatch boundaries at every slice alignment" $
      forM_ sizes $ \n ->
        forM_ [0 .. 31] $ \offset ->
          checkHarness (context n offset) b16 (sliceAt offset) (payload n)
  , testCase "every input byte across multiple vector blocks" $
      forM_ [0 .. 255] $ \byte ->
        checkHarness ("repeated byte=" ++ show byte) b16
          (sliceAt (fromIntegral byte `mod` 32)) (BS.replicate 257 byte)
  , testCase "every non-hex byte is rejected before and after vector prefixes" $
      forM_ invalidBytes $ \byte ->
        forM_ [0, 1, 15, 16, 31, 32, 63, 64, 127, 128, 4096] $ \position ->
          checkInvalid (fromIntegral byte `mod` 32) position byte
  , testCase "every invalid lane reports its original offset after long prefixes" $
      forM_ [4096 .. 4159] $ \position ->
        forM_ [35, 128, 255] $ \byte ->
          forM_ [0, 1, 15, 31] $ \offset ->
            checkInvalid offset position byte
  , testCase "odd length takes precedence even after SIMD-sized input" $
      forM_ [0, 16, 32, 64, 128, 256, 4096] $ \prefixLength -> do
        let input = BS.replicate prefixLength 48 <> "#00"
        assertEqual "odd parser input" (Left "invalid bytestring size")
          (B16.parseBase16 input)
        assertEqual "odd decoder input" (Left "invalid bytestring size")
          (B16.decodeBase16Untyped input)
        assertEqual "odd short parser input" (Left "invalid bytestring size")
          (S16.parseBase16 (SBS.toShort input))
        assertEqual "odd short decoder input" (Left "invalid bytestring size")
          (S16.decodeBase16Untyped (SBS.toShort input))
  , testCase "lazy chunks split pairs across vector blocks" $
      forM_ [15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 4096] $ \n ->
        forM_ [1, 2, 3, 7, 15, 16, 17, 31, 32, 63, 64, 127] $ \chunkSize ->
          checkHarness ("lazy length=" ++ show n ++ ", chunk=" ++ show chunkSize)
            lb16 (LBS.fromChunks . chunksOf chunkSize) (payload n)
  , testCase "short byte arrays remain compatible across boundaries and large inputs" $
      forM_ sizes $ \n ->
        checkHarness ("short length=" ++ show n) sb16 SBS.toShort (payload n)
  , testCase "short decoding reports errors in the final block and scalar tail" $
      forM_ [126, 128, 130, 254, 256, 258, 4096, 4098] $ \n ->
        forM_ [n - 33 .. n - 1] $ \position ->
          forM_ [35, 128, 255] $ \byte -> do
            let input = SBS.toShort $
                  BS.replicate position 48 <> BS.singleton byte
                    <> BS.replicate (n - position - 1) 70
                expected = Left ("invalid character at offset: " <> T.pack (show position))
                note = "short length=" ++ show n ++ ", position=" ++ show position
            assertEqual (note ++ ": decoder") expected (S16.decodeBase16Untyped input)
            assertEqual (note ++ ": parser") expected (S16.parseBase16 input)
  , testCase "short codecs retain unpinned arrays under GC pressure" $
      bracket (forkIO (forever (performGC >> threadDelay 1000))) killThread $ \_ ->
        forM_ [0 .. 31] $ \byte ->
          forM_ [64, 65, 127, 128, 129, 511, 1023] $ \n -> do
            let raw = BS.map (+ byte) (payload n)
                input = SBS.pack (BS.unpack raw)
                encoded = extractBase16 (S16.encodeBase16' input)
                decoded = S16.decodeBase16 (assertBase16 encoded)
                note = "unpinned length=" ++ show n ++ ", byte=" ++ show byte
            assertEqual (note ++ ": source is unpinned") False (isPinned input)
            assertEqual (note ++ ": encoded output is unpinned") False (isPinned encoded)
            assertEqual (note ++ ": decoded output is unpinned") False (isPinned decoded)
            checkHarness note sb16 (SBS.pack . BS.unpack) raw
            assertEqual (note ++ ": source unchanged") raw (SBS.fromShort input)
  , testCase "text wrappers preserve long Unicode payloads" textWrappers
  , localOption (QuickCheckTests 40) $
      testProperty "large random payloads agree with the independent codec" $
        forAll largePayload $ \raw ->
          let encoded = Bos.encode raw
              shortRaw = SBS.toShort raw
              shortEncoded = SBS.toShort encoded
          in conjoin
            [ extractBase16 (B16.encodeBase16' raw) === encoded
            , B16.decodeBase16 (assertBase16 encoded) === raw
            , B16.decodeBase16Untyped encoded === Right raw
            , fmap extractBase16 (B16.parseBase16 encoded) === Right encoded
            , extractBase16 (S16.encodeBase16' shortRaw) === shortEncoded
            , S16.decodeBase16 (assertBase16 shortEncoded) === shortRaw
            , S16.decodeBase16Untyped shortEncoded === Right shortRaw
            , fmap extractBase16 (S16.parseBase16 shortEncoded) === Right shortEncoded
            ]
  ]
  where
    sizes = [0 .. 5]
      ++ concatMap (\n -> [n - 2 .. n + 2]) [8, 16, 32, 64, 128, 256]
      ++ [4095, 4096, 4097, 65535, 65536, 65537]
    largePayload = do
      n <- choose (4096, 65536)
      BS.pack <$> vectorOf n arbitrary

isPinned :: SBS.ShortByteString -> Bool
isPinned (SBSI.SBS bytes) = isTrue# (isByteArrayPinned# bytes)

-- Invalid surrounding bytes ensure the codec respects the slice's bounds.
sliceAt :: Int -> BS.ByteString -> BS.ByteString
sliceAt offset bytes = BS.take (BS.length bytes) . BS.drop offset $
  BS.replicate offset 0xe5 <> bytes <> BS.replicate 32 0xff

payload :: Int -> BS.ByteString
payload n = BS.pack (take n (cycle [0 .. 255]))

context :: Int -> Int -> String
context n offset = "length=" ++ show n ++ ", slice offset=" ++ show offset

checkHarness
  :: forall a b proxy. Harness a b
  => String
  -> proxy a
  -> (BS.ByteString -> b)
  -> BS.ByteString
  -> Assertion
checkHarness note _ convert raw = do
  assertEqual (note ++ ": encode") (convert encoded)
    (extractBase16 (encode @a input))
  forM_ [("lower", encoded), ("upper", upper), ("mixed", mixed)] $
    \(name, representation) -> do
      let expected = convert representation
      assertEqual (note ++ ": " ++ name ++ " typed decode") input
        (decodeTyped @a (assertBase16 expected))
      assertEqual (note ++ ": " ++ name ++ " untyped decode") (Right input)
        (decode @a expected)
      assertEqual (note ++ ": " ++ name ++ " parser preserves case") (Right expected)
        (fmap extractBase16 (parse @a expected))
  where
    input = convert raw
    encoded = Bos.encode raw
    upper = BS8.map toUpper encoded
    mixed = BS.pack (zipWith chooseCase [0 :: Int ..] (BS.unpack encoded))
    chooseCase i byte
      | even i && byte >= 97 && byte <= 102 = byte - 32
      | otherwise = byte

invalidBytes :: [Word8]
invalidBytes = filter (`BS.notElem` "0123456789abcdefABCDEF") [0 .. 255]

checkInvalid :: Int -> Int -> Word8 -> Assertion
checkInvalid offset position byte = do
  assertEqual (note ++ ": parser") (Left expected) (B16.parseBase16 input)
  assertEqual (note ++ ": decoder") (Left expected) (B16.decodeBase16Untyped input)
  assertEqual (note ++ ": short parser") (Left expected)
    (S16.parseBase16 (SBS.toShort input))
  assertEqual (note ++ ": short decoder") (Left expected)
    (S16.decodeBase16Untyped (SBS.toShort input))
  where
    -- A second invalid byte ensures that the first bad lane wins. The valid
    -- suffix keeps the first error out of the scalar-only tail.
    input = sliceAt offset $
      BS.replicate position 48 <> BS.singleton byte
        <> BS.replicate 65 70 <> "#" <> BS.replicate suffixLength 97
    suffixLength = if even position then 257 else 256
    expected = "invalid character at offset: " <> T.pack (show position)
    note = context (BS.length input) offset
      ++ ", invalid byte=" ++ show byte ++ " at=" ++ show position

chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf n bytes
  | BS.null bytes = []
  | otherwise = let (chunk, rest) = BS.splitAt n bytes
                in sliceAt 1 chunk : chunksOf n rest

textWrappers :: Assertion
textWrappers =
  forM_ [0, 1, 15, 16, 31, 32, 127, 1024] $ \repetitions -> do
    let value = T.replicate repetitions "a\x00e9\x03bb\x1f642"
        raw = T.encodeUtf8 value
        note = "text repetitions=" ++ show repetitions
    checkHarness note t16 T.decodeUtf8 raw
    checkHarness note ts16 (TS.fromText . T.decodeUtf8) raw
    forM_ [1, 3, 15, 16, 63, 64] $ \chunkSize ->
      checkHarness (note ++ ", text chunk=" ++ show chunkSize) tl16
        (TL.fromChunks . T.chunksOf chunkSize . T.decodeUtf8) raw
