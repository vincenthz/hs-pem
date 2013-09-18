{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.PEM.Parser
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Parse PEM content.
--
-- A PEM contains contains from one to many PEM sections.
-- Each section contains an optional key-value pair header
-- and a binary content encoded in base64.
-- 
module Data.PEM.Parser
    ( pemParseBS
    , pemParseLBS
    ) where

import Data.Either (partitionEithers)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Base64.Lazy as Base64

import Data.PEM.Types

parseOnePEM :: [L.ByteString] -> Either (Maybe String) (PEM, [L.ByteString])
parseOnePEM = findPem
  where beginMarker = "-----BEGIN "
        endMarker   = "-----END "

        findPem []     = Left Nothing
        findPem (l:ls) = case beginMarker `prefixEat` l of
                             Nothing -> findPem ls
                             Just n  -> getPemName getPemHeaders n ls
        getPemName next n ls =
            let (name, r) = L.break (== 0x2d) n in
            case r of
                "-----" -> next (LC.unpack name) ls
                _       -> Left $ Just "invalid PEM delimiter found"

        getPemHeaders name lbs =
            case getPemHeaderLoop lbs of
                Left err           -> Left err
                Right (hdrs, lbs2) -> getPemContent name hdrs [] lbs2
          where getPemHeaderLoop []     = Left $ Just "invalid PEM: no more content in header context"
                getPemHeaderLoop (r:rs) = -- FIXME doesn't properly parse headers yet
                    Right ([], r:rs)

        getPemContent name hdrs contentLines lbs =
            case lbs of
                []     -> Left $ Just "invalid PEM: no end marker found"
                (l:ls) -> case endMarker `prefixEat` l of
                              Nothing ->
                                  let content = Base64.decodeLenient l
                                   in getPemContent name hdrs (content : contentLines) ls
                              Just n  -> getPemName (finalizePem name hdrs contentLines) n ls
        finalizePem name hdrs contentLines nameEnd lbs
            | nameEnd /= name = Left $ Just "invalid PEM: end name doesn't match start name"
            | otherwise       =
                let pem = PEM { pemName    = name
                              , pemHeader  = hdrs
                              , pemContent = toSBS $ L.concat $ reverse contentLines }
                 in Right (pem, lbs)

        toSBS = BC.concat . L.toChunks

        prefixEat prefix x =
            let (x1, x2) = L.splitAt (L.length prefix) x
             in if x1 == prefix then Just x2 else Nothing

-- | parser to get PEM sections
pemParse :: [L.ByteString] -> [Either String PEM]
pemParse l
    | null l    = []
    | otherwise = case parseOnePEM l of
                        Left Nothing         -> []
                        Left (Just err)      -> [Left err]
                        Right (p, remaining) -> Right p : pemParse remaining

-- | parse a PEM content using a strict bytestring
pemParseBS :: ByteString -> Either String [PEM]
pemParseBS b = pemParseLBS $ L.fromChunks [b]

-- | parse a PEM content using a dynamic bytestring
pemParseLBS :: L.ByteString -> Either String [PEM]
pemParseLBS bs = case partitionEithers $ pemParse $ LC.lines bs of
                    (x:_,_   ) -> Left x
                    ([] ,pems) -> Right pems
