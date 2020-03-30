{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Main
( main
, tests
) where


import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import "base16" Data.ByteString.Base16 as B16
import "base16" Data.ByteString.Lazy.Base16 as B16L
import "memory" Data.ByteArray.Encoding as Mem
import Data.ByteString.Random (random)
import Data.Functor (void)
import Data.Text (pack)

import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base16 Tests"
    [ testVectors
    , sanityTests
    , alphabetTests
    , lenientTests
    ]

testVectors :: TestTree
testVectors = testGroup "RFC 4648 Test Vectors"
    [ testGroup "strict encode/decode"
      [ testCaseB16 "" ""
      , testCaseB16 "f" "66"
      , testCaseB16 "fo" "666f"
      , testCaseB16 "foo" "666f6f"
      , testCaseB16 "foob" "666f6f62"
      , testCaseB16 "fooba" "666f6f6261"
      , testCaseB16 "foobar" "666f6f626172"
      ]
    , testGroup "lazy encode/decode"
      [ testCaseB16L "" ""
      , testCaseB16L "f" "66"
      , testCaseB16L "fo" "666f"
      , testCaseB16L "foo" "666f6f"
      , testCaseB16L "foob" "666f6f62"
      , testCaseB16L "fooba" "666f6f6261"
      , testCaseB16L "foobar" "666f6f626172"
      ]
    ]
  where
    testCaseB16 s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        let t' = B16.encodeBase16' s
            s' = B16.decodeBase16 t'

        step "compare encoding"
        t @=? t'

        step "compare decoding"
        Right s @=? s'

    testCaseB16L s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        let t' = B16L.encodeBase16' s
            s' = B16L.decodeBase16 t'

        step "compare encoding"
        t @=? t'

        step "compare decoding"
        Right s @=? s'

sanityTests :: TestTree
sanityTests = testGroup "Sanity tests"
    [ testGroup "strict"
      [ testGroup "very large bytestrings don't segfault"
        [ chonk
        ]
      , testGroup "`memory` sanity checks"
        [ compare 3
        , compare 4
        , compare 5
        , compare 6
        , compare 1000
        , compare 100000
        ]
      , testGroup "roundtrip encode/decode"
        [ roundtrip 3
        , roundtrip 4
        , roundtrip 5
        , roundtrip 1000
        , roundtrip 100000
        ]
      ]
    , testGroup "lazy"
      [ testGroup "very large bytestrings don't segfault"
        [ chonkL
        ]
      , testGroup "`memory` sanity checks"
        [ compareL 3
        , compareL 4
        , compareL 5
        , compareL 6
        , compareL 1000
        , compareL 100000
        ]
      , testGroup "roundtrip encode/decode"
        [ roundtripL 3
        , roundtripL 4
        , roundtripL 5
        , roundtripL 1000
        , roundtripL 100000
        ]
      ]
    ]
  where
    chonk = testCase ("Encoding huge bytestrings doesn't result in OOM or segfault") $ do
      bs <- random 1000000
      void $ return $ B16.encodeBase16' bs

    chonkL = testCase ("Encoding huge bytestrings doesn't result in OOM or segfault") $ do
      bs <- fromStrict <$> random 1000000
      void $ return $ B16L.encodeBase16' bs

    compare n = testCase ("Testing " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B16.encodeBase16' bs @=? Mem.convertToBase Mem.Base16 bs

      B16.decodeBase16 (B16.encodeBase16' bs) @=?
        first pack (Mem.convertFromBase @ByteString Mem.Base16 (Mem.convertToBase Mem.Base16 bs))

    compareL n = testCase ("Testing " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B16L.encodeBase16' (fromStrict bs) @=? fromStrict (Mem.convertToBase Mem.Base16 bs)

      B16L.decodeBase16 (B16L.encodeBase16' (fromStrict bs)) @=?
        bimap pack fromStrict (Mem.convertFromBase @ByteString Mem.Base16 (Mem.convertToBase Mem.Base16 bs))

    roundtrip n = testCase ("Roundtrip encode/decode for " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B16.decodeBase16 (B16.encodeBase16' bs) @=? Right bs

    roundtripL n = testCase ("Roundtrip encode/decode for " ++ show n ++ "-sized bytestrings") $ do
      bs <- fromStrict <$> random n
      B16L.decodeBase16 (B16L.encodeBase16' bs) @=? Right bs

alphabetTests :: TestTree
alphabetTests = testGroup "Alphabet tests"
    [ testGroup "Strict"
      [ conforms 0
      , conforms 4
      , conforms 5
      , conforms 6
      , conforms 32
      , conforms 33
      , conforms 1001
      ]
    , testGroup "Lazy"
      [ conformsL 0
      , conformsL 4
      , conformsL 5
      , conformsL 6
      , conformsL 32
      , conformsL 33
      , conformsL 1001
      ]
    ]
  where
    conforms n = testCase ("Conforms to Base16 alphabet: " ++ show n) $ do
      bs <- random n
      let b = B16.encodeBase16' bs
      assertBool ("failed validity: " ++ show b) $ B16.isValidBase16 b
      assertBool ("failed correctness: " ++ show b) $ B16.isBase16 b

    conformsL n = testCase ("Conforms to Base16 alphabet: " ++ show n) $ do
      bs <- fromStrict <$> random n
      let b = B16L.encodeBase16' bs
      assertBool ("failed validity: " ++ show b) $ B16L.isValidBase16 b
      assertBool ("failed correctness: " ++ show b) $ B16L.isBase16 b

lenientTests :: TestTree
lenientTests = testGroup "Lenient Tests"
    [ testGroup "strict encode/lenient decode"
      [ testCaseB16 "" ""
      , testCaseB16 "f" "6+6"
      , testCaseB16 "fo" "6$6+6|f"
      , testCaseB16 "foo" "==========6$$66()*f6f"
      , testCaseB16 "foob" "66^%$&^6f6f62"
      , testCaseB16 "fooba" "666f()*#@6f#)(@*)6()*)2()61"
      , testCaseB16 "foobar" "6@6@6@f@6@f@6@2@6@1@7@2++++++++++++++++++++++++"
      ]
    , testGroup "lazy encode/decode"
      [ testCaseB16L "" ""
      , testCaseB16L "f" "6+++++++____++++++======*%$@#%#^*$^6"
      , testCaseB16L "fo" "6$6+6|f"
      , testCaseB16L "foo" "==========6$$66()*f6f"
      , testCaseB16L "foob" "66^%$&^6f6f62"
      , testCaseB16L "fooba" "666f()*#@6f#)(@*)6()*)2()61"
      , testCaseB16L "foobar" "6@6@6@f@6@f@6@2@6@1@7@2++++++++++++++++++++++++"
      ]
    ]
  where
    testCaseB16 s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        let t0 = B16.decodeBase16 (B16.encodeBase16' s)
            t1 = B16.decodeBase16Lenient t

        step "compare decoding"
        t0 @=? Right t1

    testCaseB16L s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        let t0 = fmap toStrict $ B16L.decodeBase16 (B16L.encodeBase16' s)
            t1 = Right . toStrict $ B16L.decodeBase16Lenient t

        step "compare decoding"
        t0 @=? t1
