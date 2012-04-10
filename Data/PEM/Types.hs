-- |
-- Module      : Data.PEM.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Data.PEM.Types where

import Data.ByteString (ByteString)

-- | Represent one PEM section
--
-- for now headers are not serialized at all.
-- this is just available here as a placeholder for a later implementation.
data PEM = PEM
        { pemName    :: String                 -- ^ the name of the section, found after the dash BEGIN tag.
        , pemHeader  :: [(String, ByteString)] -- ^ optionals key value pair header
        , pemContent :: ByteString             -- ^ binary content of the section
        } deriving (Show,Eq)
