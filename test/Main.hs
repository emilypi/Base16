{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2020-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the test implementation for the `base16` package
--
module Main
( main
, tests
) where


import Control.Monad (forM_)
import Data.Base16.Types
import Data.Bifunctor (second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified "base16" Data.ByteString.Base16 as B16
import qualified "base16" Data.ByteString.Lazy.Base16 as LB16
import qualified "base16" Data.ByteString.Short.Base16 as SB16
import qualified "base16-bytestring" Data.ByteString.Base16 as Bos
import Data.Char (chr)
import Data.Either (isLeft, isRight)
import qualified Data.List as L
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified "base16" Data.Text.Encoding.Base16 as T16
import Data.Text.Encoding.Base16.Error (Base16Error(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified "base16" Data.Text.Lazy.Encoding.Base16 as TL16
import qualified Data.Text.Short as TS
import qualified "base16" Data.Text.Short.Encoding.Base16 as TS16

import Internal
import qualified Simd

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Positive(..))


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base16 Tests"
  [ mkTree b16
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree lb16
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree sb16
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree t16
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree T.decodeUtf8' b16
    ]
  , mkTree tl16
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree TL.decodeUtf8' lb16
    ]
  , mkTree ts16
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree
      (second TS.fromText . T.decodeUtf8' . SBS.fromShort) sb16
    ]
  , testCase "parseBase16 accepts exactly the ASCII hex alphabet" $
    assertBool "accepts invalid hex" $
      isRight (B16.parseBase16 valids) && all (isLeft . B16.parseBase16) invalids
  , prop_bos_coherence
  , parserRegressions
  , lenientTextRegressions
  , Simd.tests
  ]

valids :: BS.ByteString
valids = fromString hexChars

hexChars :: String
hexChars = "0123456789abcdefABCDEF"

-- This is to verify EVERY non-hex char is "not valid"
-- The bytestrings have the same character twice, so that
-- the parser does not reject the input
-- because of the odd number of bytes.
invalids :: [BS.ByteString]
invalids = fromString <$> separate8s
  where
    allWord8 = chr <$> [0..255]
    nonHex = L.filter (`L.notElem` hexChars) allWord8
    separate8s = (\c -> [c,c]) <$> nonHex

-- ---------------------------------------------------------------- --
-- Test tree generation

-- | Make a test tree for a given label
--
mkTree
  :: forall a b proxy
  . Harness a b
  => proxy a
  -> [proxy a -> TestTree]
  -> TestTree
mkTree a = testGroup (label @a) . fmap ($ a)

-- | Make a test group with some name, lifting a test tree up to the correct
-- type information via some Harness
--
mkTests
  :: forall a b proxy
  . Harness a b
  => String
  -> [proxy a -> TestTree]
  -> proxy a
  -> TestTree
mkTests context ts = testGroup context . (<*>) ts . pure

-- | Make property tests for a given harness instance
--
mkPropTree :: forall a b proxy. Harness a b => proxy a -> TestTree
mkPropTree = mkTests "Property Tests"
  [ prop_roundtrip
  , prop_untyped_correctness
  ]

-- | Make unit tests for a given harness instance
--
mkUnitTree
  :: forall a b proxy
  . Harness a b
  => proxy a
  -> TestTree
mkUnitTree = mkTests "Unit tests"
  [ rfcVectors
  , parserVectors
  , lenientTests
  ]

-- | Make unit tests for textual 'decode*With' functions
--
mkDecodeTree
  :: forall t a b c e proxy
  . ( TextHarness a b c
    , Harness t c
    , Show e
    )
  => (c -> Either e b)
  -> proxy t
  -> proxy a
  -> TestTree
mkDecodeTree utf8 t = mkTests "Decoding tests"
  [ decodeWithVectors utf8 t
  ]

-- ---------------------------------------------------------------- --
-- Property tests

prop_roundtrip :: forall a b proxy. Harness a b => proxy a -> TestTree
prop_roundtrip _ = testGroup "prop_roundtrip"
  [ testProperty "prop_std_roundtrip_typed" $ \(bs :: b) ->
      bs == decodeTyped (encode bs)
  , testProperty "prop_std_roundtrip_untyped" $ \(bs :: b) ->
      Right bs
        == decode (extractBase16 $ encode bs)
  , testProperty "prop_std_lenient_roundtrip" $ \(bs :: b) ->
      bs == lenient (extractBase16 $ encode bs)
  ]

prop_untyped_correctness :: forall a b proxy. Harness a b => proxy a -> TestTree
prop_untyped_correctness _ = testGroup "prop_validity"
  [ testProperty "parsing an encoding preserves the encoded representation" $ \(bs :: b) ->
      let encoded = encode bs
      in Right encoded == parse (extractBase16 encoded)
  , testProperty "successful parsing preserves its input" $ \(candidate :: b) ->
      case parse candidate of
        Left _ -> True
        Right encoded -> extractBase16 encoded == candidate
  ]

-- | just a sanity check against `base16-bytestring`
--
prop_bos_coherence :: TestTree
prop_bos_coherence = testGroup "prop_bos_coherence"
  [ testProperty "prop_std_bos_coherence" $ \bs ->
      let encoded = Bos.encode bs
      in encoded == extractBase16 (B16.encodeBase16' bs)
        && Right bs == B16.decodeBase16Untyped encoded
        && bs == B16.decodeBase16 (assertBase16 encoded)
  ]

-- ---------------------------------------------------------------- --
-- Unit tests

parserVectors :: forall a b proxy. Harness a b => proxy a -> TestTree
parserVectors _ = testGroup "Parser contract"
  [ testCase "accepts empty, upper-case, lower-case and mixed-case encodings" $
      forM_ ["", "00", "0123456789aAbBcCdDeEfF", "ff", "FF"] $ \input ->
        Right input @=? fmap extractBase16 (parse @a input)
  , testCase "odd length takes precedence over invalid characters" $
      forM_ ["0", "z", "z00", "0z0"] $ \input ->
        Left "invalid bytestring size" @=? parse @a input
  , testCase "reports the first invalid character" $
      forM_ [("gg", 0 :: Int), ("0z", 1), ("00z0", 2), ("00zz", 2)] $
        \(input, offset) ->
          Left (invalidAt offset) @=? parse @a input
  ]

parserRegressions :: TestTree
parserRegressions = testGroup "Parser and lazy chunk regressions"
  [ testProperty "strict byte parsing and untyped decoding agree on arbitrary input" $
      \candidate ->
        fmap B16.decodeBase16 (B16.parseBase16 candidate)
          == B16.decodeBase16Untyped candidate
  , testProperty "short byte parsing and untyped decoding agree on arbitrary input" $
      \candidate ->
        fmap SB16.decodeBase16 (SB16.parseBase16 candidate)
          == SB16.decodeBase16Untyped candidate
  , testProperty "lazy errors and decoding are independent of arbitrary chunk boundaries" $
      \candidate sizes ->
        let chunked = LBS.fromChunks (chunksBy sizes candidate)
            parsed = fmap (LBS.toStrict . extractBase16) (LB16.parseBase16 chunked)
        in parsed == fmap extractBase16 (B16.parseBase16 candidate)
          && fmap LBS.toStrict (LB16.decodeBase16Untyped chunked)
            == B16.decodeBase16Untyped candidate
  , testProperty "parsing preserves lazy byte chunks and typed decoding joins split pairs" $
      \raw sizes ->
        let chunked = LBS.fromChunks (chunksBy sizes (Bos.encode raw))
        in fmap (LBS.toChunks . extractBase16) (LB16.parseBase16 chunked)
              == Right (LBS.toChunks chunked)
            && LB16.decodeBase16 (assertBase16 chunked) == LBS.fromStrict raw
  , testProperty "parsing preserves lazy text chunks and typed decoding joins split pairs" $
      \raw sizes ->
        let encoded = Bos.encode (T.encodeUtf8 raw)
            chunked = TL.fromChunks (map T.decodeUtf8 (chunksBy sizes encoded))
        in fmap (TL.toChunks . extractBase16) (TL16.parseBase16 chunked)
              == Right (TL.toChunks chunked)
            && TL16.decodeBase16 (assertBase16 chunked) == TL.fromStrict raw
  , testCase "every byte split preserves case and decodes pairs crossing a chunk" $
      forM_ ["", "fF", "aAbBcCdDeEfF", Bos.encode (BS.pack [0 .. 255])] $ \encoded ->
        forM_ [0 .. BS.length encoded] $ \split -> do
          let chunked = LBS.fromChunks [BS.take split encoded, BS.drop split encoded]
              note = "encoded=" ++ show encoded ++ ", split=" ++ show split
          assertEqual (note ++ ": parse") (Right (LBS.toChunks chunked))
            (fmap (LBS.toChunks . extractBase16) (LB16.parseBase16 chunked))
          expected <- either assertFailure pure (Bos.decode encoded)
          assertEqual (note ++ ": typed decode") expected
            (LBS.toStrict (LB16.decodeBase16 (assertBase16 chunked)))
          assertEqual (note ++ ": untyped decode") (Right expected)
            (fmap LBS.toStrict (LB16.decodeBase16Untyped chunked))
  , testCase "every text split preserves chunks and decodes Unicode payloads" $ do
      let raw = "a\x00e9\x03bb\x1f642"
          encoded = T.decodeUtf8 (Bos.encode (T.encodeUtf8 raw))
      forM_ [0 .. T.length encoded] $ \split -> do
        let chunked = TL.fromChunks [T.take split encoded, T.drop split encoded]
        Right (TL.toChunks chunked)
          @=? fmap (TL.toChunks . extractBase16) (TL16.parseBase16 chunked)
        TL.fromStrict raw @=? TL16.decodeBase16 (assertBase16 chunked)
        Right (TL.fromStrict raw) @=? TL16.decodeBase16Untyped chunked
  , testCase "lazy invalid offsets are global and odd total length has priority" $
      forM_ [("66z0", invalidAt 2), ("666f0zz0", invalidAt 5),
             ("z00", "invalid bytestring size")] $ \(input, expected) ->
        forM_ [0 .. BS.length input] $ \split -> do
          let chunked = LBS.fromChunks [BS.take split input, BS.drop split input]
              text = TL.fromChunks (map T.decodeUtf8 (LBS.toChunks chunked))
          Left expected @=? LB16.parseBase16 chunked
          Left expected @=? LB16.decodeBase16Untyped chunked
          Left expected @=? TL16.parseBase16 text
          Left expected @=? TL16.decodeBase16Untyped text
  , testCase "text parsing checks encoded bytes without decoding invalid UTF-8 payloads" $
      forM_ ["ff", "c0af", "eda080", "f4908080"] $ \input -> do
        Right input @=? fmap extractBase16 (T16.parseBase16 input)
        Right (TS.fromText input) @=? fmap extractBase16 (TS16.parseBase16 (TS.fromText input))
        let chunked = TL.fromChunks (T.chunksOf 1 input)
        Right (TL.toChunks chunked)
          @=? fmap (TL.toChunks . extractBase16) (TL16.parseBase16 chunked)
  , testCase "text diagnostics use UTF-8 byte lengths and reject non-ASCII hex lookalikes" $
      forM_ [("\x00e9", invalidAt 0), ("0\x20ac", invalidAt 1),
             ("00\x1f4a9", invalidAt 2), ("\xff10\xff11", invalidAt 0),
             ("f\x03bb", "invalid bytestring size"),
             ("\x00e9\&0", "invalid bytestring size")] $ \(input, expected) -> do
        Left expected @=? T16.parseBase16 input
        Left expected @=? TS16.parseBase16 (TS.fromText input)
        Left expected @=? TL16.parseBase16 (TL.fromChunks (T.chunksOf 1 input))
  ]

invalidAt :: Int -> T.Text
invalidAt offset = "invalid character at offset: " <> T.pack (show offset)

lenientTextRegressions :: TestTree
lenientTextRegressions = testGroup "Lenient text Unicode regressions"
  [ testCase "issue 32: Latin-1 byte df decodes to sharp s" $
      checkText "0014df1047454e53" "\NUL\DC4\xdf\DLEGENS"
  , testCase "each isolated byte has a defined character result" $
      forM_ [0 .. 255] $ \byte ->
        checkText (T.decodeUtf8 (Bos.encode (BS.singleton byte)))
          (T.singleton (chr (fromIntegral byte)))
  , testCase "valid UTF-8 survives beside invalid and truncated sequences" $
      forM_ [("c3a9dff09f9982", "\xe9\xdf\x1f642"),
             ("e282", "\xe2\x82"),
             ("e228a1", "\xe2(\xa1"),
             ("c0af", "\xc0\xaf"),
             ("eda080", "\xed\xa0\x80"),
             ("f4908080", "\xf4\x90\x80\x80"),
             ("c3a9#d!f0", "\xe9\xdf")] $ \(input, expected) ->
        checkText input expected
  , testCase "lazy decoding keeps UTF-8 sequences across every hex split" $
      forM_ [("0014df1047454e53", "\NUL\DC4\xdf\DLEGENS"),
             ("c3a9dff09f9982e282", "\xe9\xdf\x1f642\xe2\x82")] $ \(input, expected) ->
        forM_ [0 .. T.length input] $ \split -> do
          let chunked = TL.fromChunks [T.take split input, T.drop split input]
          expected @=? TL.toStrict (TL16.decodeBase16Lenient chunked)
  , testProperty "arbitrary decoded bytes yield valid UTF-8 in all text variants" $
      \raw ->
        let input = T.decodeUtf8 (Bos.encode raw)
            strict = T16.decodeBase16Lenient input
            lazy = TL.toStrict (TL16.decodeBase16Lenient (TL.fromChunks (T.chunksOf 1 input)))
            short = TS16.decodeBase16Lenient (TS.fromText input)
        in Right strict == T.decodeUtf8' (T.encodeUtf8 strict)
          && strict == lazy
          && Right strict == T.decodeUtf8' (TS.toByteString short)
  ]
  where
    checkText input expected = do
      expected @=? T16.decodeBase16Lenient input
      expected @=? TL.toStrict (TL16.decodeBase16Lenient (TL.fromStrict input))
      let short = TS16.decodeBase16Lenient (TS.fromText input)
      -- Validate the underlying bytes, not merely an unsafe text wrapper.
      Right expected @=? T.decodeUtf8' (TS.toByteString short)

-- Vary chunk boundaries independently of the payload, retaining a final chunk
-- when the generated sizes run out. The bound keeps generated chunks useful.
chunksBy :: [Positive Int] -> BS.ByteString -> [BS.ByteString]
chunksBy _ bytes | BS.null bytes = []
chunksBy [] bytes = [bytes]
chunksBy (Positive size : sizes) bytes =
  let (chunk, rest) = BS.splitAt (1 + size `mod` 97) bytes
  in chunk : chunksBy sizes rest

rfcVectors :: forall a b proxy. Harness a b => proxy a -> TestTree
rfcVectors _ = testGroup "RFC 4648 Test Vectors"
    [ testGroup "lower-case"
      [ testCaseB16 "" ""
      , testCaseB16 "f" "66"
      , testCaseB16 "fo" "666f"
      , testCaseB16 "foo" "666f6f"
      , testCaseB16 "foob" "666f6f62"
      , testCaseB16 "fooba" "666f6f6261"
      , testCaseB16 "foobar" "666f6f626172"
      ]
    ,  testGroup "upper-case"
      [ testCaseB16 "" ""
      , testCaseB16 "f" "66"
      , testCaseB16 "fo" "666F"
      , testCaseB16 "foo" "666F6F"
      , testCaseB16 "foob" "666F6F62"
      , testCaseB16 "fooba" "666F6F6261"
      , testCaseB16 "foobar" "666F6F626172"
      ]
    ,  testGroup "mixed-case"
      [ testCaseB16 "" ""
      , testCaseB16 "f" "66"
      , testCaseB16 "fo" "666F"
      , testCaseB16 "foo" "666F6f"
      , testCaseB16 "foob" "666F6f62"
      , testCaseB16 "fooba" "666F6f6261"
      , testCaseB16 "foobar" "666F6f626172"
      ]
    ]
  where
    testCaseB16 s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do

        step "encode is sound"
        lower t @=? extractBase16 (encode @a s)

        step "decode is sound"
        s @=? decodeTyped (assertBase16 t)

        step "parse preserves the supplied case"
        Right t @=? fmap extractBase16 (parse @a t)

        step "decodeUntyped is sound"
        Right s @=? decode (extractBase16 $ encode s)

-- | Unit test trees for the `decode*With` family of text-valued functions
--
decodeWithVectors
  :: forall t a b c e proxy
  . ( TextHarness a c b
    , Harness t b
    , Show e
    )
  => (b -> Either e c)
    -- ^ utf8
  -> proxy t
    -- ^ witness to the bytestring-ey dictionaries
  -> proxy a
    -- ^ witness to the text dictionaries
  -> TestTree
decodeWithVectors utf8 _ _ = testGroup "DecodeWith* unit tests"
  [ testGroup "decodeWith negative tests"
    [ testCase "decodeWith non-utf8 inputs on decodeUtf8" $ do
      case decodeWith_ @a utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeWith valid utf8 inputs on decodeUtf8" $ do
      case decodeWith_ @a utf8 (extractBase16 $ encode @t "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    ]
  , testGroup "decodeWith positive tests"
    [ testCase "decodeWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decode @a "666f6f626172"
      b <- either (assertFailure . show) pure $ decodeWith_ @a utf8 "666f6f626172"
      a @=? b
    ]
  ]

lenientTests :: forall a b proxy. Harness a b => proxy a -> TestTree
lenientTests _ = testGroup "Lenient Tests"
    [ testCaseB16 "" ""
    , testCaseB16 "f" "6+6"
    , testCaseB16 "fo" "6$6+6|f"
    , testCaseB16 "foo" "==========6$$66()*F6f"
    , testCaseB16 "foob" "66^%$&^6f6F62"
    , testCaseB16 "fooba" "666f()*#@6F#)(@*)6()*)2()61"
    , testCaseB16 "foobar" "6@6@6@f@6@f@6@2@6@1@7@2++++++++++++++++++++++++"
    ]
  where
    testCaseB16 s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        let t0 = decode (extractBase16 $ encode @a s)
            t1 = lenient @a t

        step "compare decoding"
        t0 @=? Right t1
