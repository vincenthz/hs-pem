{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.PEM.Writer
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Data.PEM.Writer
    ( pemWriteBuilder
    , pemWriteLBS
    , pemWriteBS
    ) where

import Data.PEM.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64 as Base64
import Data.Serialize.Builder
import Data.Monoid
import Data.List

-- | write a PEM structure to a builder
pemWriteBuilder :: PEM -> Builder
pemWriteBuilder pem = mconcat $ intersperse eol $ concat ([begin]:header:section:[end]:[[empty]])
    where begin   = mconcat $ map fromByteString ["-----BEGIN ", sectionName, "-----" ]
          end     = mconcat $ map fromByteString ["-----END ", sectionName, "-----" ]
          section = map fromByteString $ (splitChunks $ Base64.encode $ pemContent pem)
          header  = if null $ pemHeader pem
                        then []
                        else concatMap toHeader (pemHeader pem) ++ [empty]
          toHeader (k,v) = [ mconcat $ map fromByteString [ bk, ":", v ] ]
                where bk = BC.pack k
          -- expect only ASCII. need to find a type to represent it.
          sectionName = BC.pack $ pemName pem

          splitChunks b
                  | B.length b > 64 = let (x,y) = B.splitAt 64 b in x : splitChunks y
                  | otherwise       = [b]
          eol = fromByteString $ B.singleton 0x0a

-- | convert a PEM structure to a bytestring
pemWriteBS :: PEM -> ByteString
pemWriteBS = toByteString . pemWriteBuilder

-- | convert a PEM structure to a lazy bytestring
pemWriteLBS :: PEM -> L.ByteString
pemWriteLBS = toLazyByteString . pemWriteBuilder
