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
testMarshall pems = readPems == Right pems
    where readPems = pemParseBS writtenPems
          writtenPems = B.concat (map pemWriteBS pems)

arbitraryName = choose (1, 30) >>= \i -> replicateM i arbitraryAscii
    where arbitraryAscii = elements ['A'..'Z']

arbitraryContent = choose (1,100) >>= \i ->
                   (B.pack . map fromIntegral) `fmap` replicateM i (choose (0,255) :: Gen Int)

instance Arbitrary PEM where
    arbitrary = PEM <$> arbitraryName <*> pure [] <*> arbitraryContent
