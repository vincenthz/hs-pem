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
    ( pemParser
    , pemParseBS
    , pemParseLBS
    ) where

import Control.Applicative
import Data.Attoparsec
import qualified Data.Attoparsec.Lazy as AttoLazy
import Data.Attoparsec.Char8 (space)

import Data.PEM.Types

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64 as Base64

import Prelude hiding (takeWhile)

-- | parser to get PEM sections
pemParser :: Parser [PEM]
pemParser = many contextSection
    where
          beginMarker = string "-----BEGIN " >> return ()
          endMarker = string "-----END " >> return ()
          skipLine = skipWhile (/= 0xa) >> space >> return ()
          eatLine = takeWhile (/= 0xa) <* space
          contextSection = manyTill skipLine beginMarker *> section
          section = do
               -- begin marker has already been eaten by contextSection
               name <- takeWhile (/= 0x2d) <* (string "-----" *> space)
               l    <- manyTill eatLine endMarker
               let content = Base64.decodeLenient $ BC.concat l
               return $ PEM { pemName = BC.unpack name, pemHeader = [], pemContent = content }

-- | parse a PEM content using a strict bytestring
pemParseBS :: ByteString -> Either String [PEM]
pemParseBS = parseOnly pemParser

-- | parse a PEM content using a dynamic bytestring
pemParseLBS :: L.ByteString -> Either String [PEM]
pemParseLBS = AttoLazy.eitherResult . AttoLazy.parse pemParser
