{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
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

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either (partitionEithers)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import Data.PEM.Types
import Data.ByteArray.Encoding (Base(Base64), convertFromBase)
import qualified Data.ByteArray as BA

type Line = L.ByteString

parseOnePEM :: [Line] -> Either (Maybe String) (PEM, [Line])
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
                Right (hdrs, lbs2) -> getPemContent name hdrs initial lbs2
          where getPemHeaderLoop []     = Left $ Just "invalid PEM: no more content in header context"
                getPemHeaderLoop (r:rs) = -- FIXME doesn't properly parse headers yet
                    Right ([], r:rs)

        getPemContent name hdrs !contentLines lbs =
            case lbs of
                []     -> Left $ Just "invalid PEM: no end marker found"
                (l:ls) -> case endMarker `prefixEat` l of
                              Nothing ->
                                  let contentLines' = processOneLine contentLines l
                                   in getPemContent name hdrs contentLines' ls
                              Just n  -> getPemName (finalizePem name hdrs contentLines) n ls
        finalizePem name hdrs contentLines nameEnd lbs
            | nameEnd /= name = Left $ Just "invalid PEM: end name doesn't match start name"
            | otherwise       = do
                chunks <- reverseAndPad contentLines
                let pem = PEM { pemName    = name
                              , pemHeader  = hdrs
                              , pemContent = BA.concat chunks }
                return (pem, lbs)

        prefixEat prefix x =
            let (x1, x2) = L.splitAt (L.length prefix) x
             in if x1 == prefix then Just x2 else Nothing

        initial = ([], Nothing)

-- content parse state, holding converted chunks in reverse order and
-- optionally some base64 characters as leftovers from one line to the next
type CState = ([ByteString], Maybe ByteString)

processOneLine :: CState -> Line -> CState
processOneLine (content, leftovers) line =
    go content $ maybe lineChunks (: lineChunks) leftovers
  where
    lineChunks = L.toChunks (LC.filter goodChar line)
    goodChar c = isDigit c || isAsciiUpper c || isAsciiLower c
                           || c == '+' || c == '/'
    n = 4

    build x f l xs = let !d = strictDecode x in f (d:l) xs
    {-# INLINE build #-}

    -- adapted from base64-bytestring internal reChunkIn, but also returns
    -- base64 leftovers that could not be used at line end, and should
    -- be used at the begining of the next line, or as part of final padding

    go l []       = (l, Nothing)
    go l (y : ys) =
          case BA.length y `divMod` n of
              (_, 0) -> build y go l ys
              (0, _) -> fixup l y ys
              (d, _) -> let (prefix, suffix) = BA.splitAt (d * n) y
                         in build prefix fixup l suffix ys

    fixup l acc []       = (l, Just acc)
    fixup l acc (z : zs) =
        case BA.splitAt (n - BA.length acc) z of
            (prefix, suffix) ->
                let acc' = acc `BA.append` prefix
                 in if BA.length acc' == n
                        then let zs' = if BA.null suffix
                                           then zs
                                           else suffix : zs
                              in build acc' go l zs'
                       else -- suffix must be null
                           fixup l acc' zs

-- leftovers with 0, 2 or 3 characters are valid end sequences, but
-- only 1 character is a parse error, as this length is not valid for
-- encoded base64 content
reverseAndPad :: CState -> Either (Maybe String) [ByteString]
reverseAndPad (content, leftovers) = fmap reverse (pad leftovers content)
  where
    pad Nothing  l = Right l
    pad (Just b) l =
        case BA.length b `mod` 4 of
            3 -> Right $ strictDecode (b `BA.append` "=")  : l
            2 -> Right $ strictDecode (b `BA.append` "==") : l
            1 -> Left  $ Just "invalid PEM: missing or extra base64 characters"
            _ -> Right $ strictDecode b : l

-- base64 decoding that should never fail, as it is fed only with
-- sequences of valid characters whose length is a multiple of 4
strictDecode :: ByteString -> ByteString
strictDecode x =
    case convertFromBase Base64 x of
        Left  _ -> error "parseOnePEM: invalid chunk detected"
        Right r -> r
{-# INLINE strictDecode #-}

-- | parser to get PEM sections
pemParse :: [Line] -> [Either String PEM]
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
pemParseLBS bs = case partitionEithers $ pemParse $ map unCR $ LC.lines bs of
                    (x:_,_   ) -> Left x
                    ([] ,pems) -> Right pems
  where unCR b | L.length b > 0 && L.last b == cr = L.init b
               | otherwise                        = b
        cr = fromIntegral $ fromEnum '\r'
