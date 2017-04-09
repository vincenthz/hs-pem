module Main where

import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Char8 as BC
import Test.QuickCheck hiding ((.&.))
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
    [ testGroup "units" $ testUnits
    , testDecodingMultiple
    , testUnmatchingNames
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

testDecodingMultiple = testCase ("multiple pems") (pemParseBS content @=? Right expected)
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
            , "A"
            , "AE"
            , "CAw"
            , "QFBg"
            , "cICQo"
            , "LDA0OD"
            , "xAREhMU"
            , "FRYXGBka"
            , "GxwdHh8gI"
            , "SIjJCUmJyg"
            , "pKissLS4vMD"
            , "EyMzQ1Njc4OT"
            , "o7PD0+P0BBQkN"
            , "ERUZHSElKS0xNT"
            , "k9QUVJTVFVWV1hZ"
            , "WltcXV5fYGFiY2Rl"
            , "ZmdoaWprbG1ub3Bxc"
            , "nN0dXZ3eHl6e3x9fn+"
            , "AgYKDhIWGh4iJiouMjY"
            , "6PkJGSk5SVlpeYmZqbnJ"
            , "2en6ChoqOkpaanqKmqq6y"
            , "trq+wsbKztLW2t7i5uru8v"
            , "b6/wMHCw8TFxsfIycrLzM3O"
            , "z9DR0tPU1dbX2Nna29zd3t/g"
            , "4eLj5OXm5+jp6uvs7e7v8PHy8"
            , "/T19vf4+         fr7/P3+/w"
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

testMarshall pems = readPems == Right pems
    where readPems = pemParseBS writtenPems
          writtenPems = B.concat (map pemWriteBS pems)

arbitraryName = choose (1, 30) >>= \i -> replicateM i arbitraryAscii
    where arbitraryAscii = elements ['A'..'Z']

arbitraryContent = choose (1,100) >>= \i ->
                   (B.pack . map fromIntegral) `fmap` replicateM i (choose (0,255) :: Gen Int)

instance Arbitrary PEM where
    arbitrary = PEM <$> arbitraryName <*> pure [] <*> arbitraryContent
