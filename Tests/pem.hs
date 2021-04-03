module Main where

import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Char8 as BC
import Test.QuickCheck
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import Data.PEM
import qualified Data.ByteString as B

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "units" testUnits
    , testDecodingMultiple
    , testUnmatchingNames
    , testDecodingSpecialNames
    , testProperty "marshall" testMarshall
    ]

testUnits = map (\(i, (p,bs)) -> testCase (show i) (pemWriteBS p @=? BC.pack bs))
                $ zip [0..] [ (p1, bp1), (p2, bp2) ]
  where p1  = PEM { pemName = "abc", pemHeader = [], pemContent = B.replicate 64 0 }
        bp1 = unlines
                [ "-----BEGIN abc-----"
                , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                , "AAAAAAAAAAAAAAAAAAAAAA=="
                , "-----END abc-----"
                ]
        p2 = PEM { pemName = "xxx", pemHeader = [], pemContent = B.replicate 12 3 }
        bp2 = unlines
                [ "-----BEGIN xxx-----"
                , "AwMDAwMDAwMDAwMD"
                , "-----END xxx-----"
                ]

testDecodingMultiple = testCase "multiple pems" (pemParseBS content @=? Right expected)
  where expected = [ PEM { pemName = "marker", pemHeader = [], pemContent = B.replicate 12 3 }
                   , PEM { pemName = "marker2", pemHeader = [], pemContent = B.replicate 64 0 }
                   , PEM { pemName = "marker3", pemHeader = [], pemContent = B.pack [0..255] }
                   ]
        content = BC.pack $ unlines
            [ "some text that is not related to PEM"
            , "and is just going to be ignored by the PEM parser."
            , ""
            , "even empty lines should be skip until the rightful marker"
            , "-----BEGIN marker-----"
            , "AwMDAwMDAwMDAwMD"
            , "-----END marker-----"
            , "some middle text"
            , "-----BEGIN marker2-----"
            , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
            , "AAAAAAAAAAAAAAAAAAAAAA=="
            , "-----END marker2-----"
            , "also test unaligned content, missing padding and whitespace"
            , "-----BEGIN marker3-----"
            , "                                             "
            , " AAECAwQ  FBgcICQo                           "
            , "  LDA0ODx  AREhMUFR                          "
            , "   YXGBkaG  xwdHh8gI                         "
            , "    SIjJCUm  JygpKiss                        "
            , "     LS4vMDE  yMzQ1Njc  4OTo7PD0+P0BBQ       "
            , "      kNERUZH  SElKS0xN  Tk9QUVJTVFVWV1      "
            , "       hZWltcX  V5fYGFiY  2RlZmdoaWprbG1     "
            , "        ub3Bxcn  N0dXZ3eH                    "
            , "        l6e3x9f  n+AgYKDhI  WGh4iJiouMjY6P   "
            , "       kJGSk5S  VlpeYmZqbnJ  2en6ChoqOkpaan  "
            , "      qKmqq6y  trq+wsbKztLW2  t7i5uru8vb6/wM "
            , "     HCw8TFx  sfIycrL zM3Oz9D                "
            , "    R0tPU1d  bX2Nna2   9zd3t/g               "
            , "   4eLj5OX  m5+jp6u     vs7e7v8              "
            , "  PHy8/T1  9vf4+fr       7/P3+/w             "
            , "                                             "
            , "-----END marker3-----"
            , "and finally some trailing text."
            ]

testUnmatchingNames = testCase "unmatching name" (let r = pemParseBS content in case r of
                                                                                 Left _ -> True @=? True
                                                                                 _      -> r @=? Left "")
  where content = BC.pack $ unlines
            [ "-----BEGIN marker-----"
            , "AAAA"
            , "-----END marker2-----"
            ]

testDecodingSpecialNames = testCase "special names" (Right expected @=? pemParseBS content)
  where expected = [ PEM { pemName = "", pemHeader = [], pemContent = B.empty }
                   , PEM { pemName = "space char", pemHeader = [], pemContent = B.empty }
                   , PEM { pemName = "hyphen-minus", pemHeader = [], pemContent = B.empty }
                   ]
        content = BC.pack $ unlines
            [ "-----BEGIN -----"
            , "-----END -----"
            , "-----BEGIN space char-----"
            , "-----END space char-----"
            , "-----BEGIN hyphen-minus-----"
            , "-----END hyphen-minus-----"
            ]

testMarshall pems = readPems == Right pems
    where readPems = pemParseBS writtenPems
          writtenPems = B.concat (map pemWriteBS pems)

arbitraryName = choose (1, 30) >>= \i -> replicateM i arbitraryAscii
    where arbitraryAscii = elements ['A'..'Z']

arbitraryContent = choose (1,100) >>= \i ->
                   (B.pack . map fromIntegral) `fmap` replicateM i (choose (0,255) :: Gen Int)

instance Arbitrary PEM where
    arbitrary = PEM <$> arbitraryName <*> pure [] <*> arbitraryContent
