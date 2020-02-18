{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Main
( main
, tests
) where


import Data.Bifunctor
import Data.ByteString (ByteString)
import "base16" Data.ByteString.Base16 as B16
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
    ]

testVectors :: TestTree
testVectors = testGroup "RFC 4648 Test Vectors"
    [ testGroup "encode/decode"
      [ testCaseB16 "" ""
      , testCaseB16 "f" "66"
      , testCaseB16 "fo" "666f"
      , testCaseB16 "foo" "666f6f"
      , testCaseB16 "foob" "666f6f62"
      , testCaseB16 "fooba" "666f6f6261"
      , testCaseB16 "foobar" "666f6f626172"
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

sanityTests :: TestTree
sanityTests = testGroup "Sanity tests"
    [ testGroup "very large bytestrings don't segfault"
        [ chonk
        ]
    , testGroup "`memory` sanity checks"
        [ compare32 3
        , compare32 4
        , compare32 5
        , compare32 6
        , compare32 1000
        , compare32 100000
        ]
    , testGroup "roundtrip encode/decode"
        [ roundtrip 3
        , roundtrip 4
        , roundtrip 5
        , roundtrip 1000
        , roundtrip 100000
        ]
    ]
  where
    chonk = testCase ("Encoding huge bytestrings doesn't result in OOM or segfault") $ do
      bs <- random 1000000
      void $ return $ B16.encodeBase16' bs

    compare32 n = testCase ("Testing " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B16.encodeBase16' bs @=? Mem.convertToBase Mem.Base16 bs

      B16.decodeBase16 (B16.encodeBase16' bs) @=?
        first pack (Mem.convertFromBase @ByteString Mem.Base16 (Mem.convertToBase Mem.Base16 bs))

    roundtrip n = testCase ("Roundtrip encode/decode for " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B16.decodeBase16 (B16.encodeBase16' bs) @=? Right bs

alphabetTests :: TestTree
alphabetTests = testGroup "Alphabet tests"
    [ base16Tests 0
    , base16Tests 4
    , base16Tests 5
    , base16Tests 6
    ]
  where
    base16Tests n = testCase ("Conforms to Base16 alphabet: " ++ show n) $ do
      bs <- random n
      let b = B16.encodeBase16' bs
      assertBool ("failed validity: " ++ show b) $ B16.isValidBase16 b
      assertBool ("failed correctness: " ++ show b) $ B16.isBase16 b
