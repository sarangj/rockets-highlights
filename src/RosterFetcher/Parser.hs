{-# LANGUAGE OverloadedStrings #-}
module RosterFetcher.Parser where

import Prelude
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import RosterFetcher.Types

parsePlayers :: Object -> [Player]
parsePlayers o = case HashMap.lookup "rowSet" o of
  Just (Array v) -> successes $ map fromJSON $ Vector.toList v
  _ -> []
  where 
    successes :: [Result a] -> [a]
    successes xs = [ x | Success x <- xs ] 
