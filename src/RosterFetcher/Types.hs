{-# LANGUAGE OverloadedStrings #-}
module RosterFetcher.Types where

import Prelude
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!))

data Player = Player 
  { player_name :: Text
  , player_number :: Int
  , player_position :: Position
  , player_height :: Height
  , player_weight :: Int
  , player_birthDate :: Text
  , player_age :: Double
  , player_exp :: Experience
  , player_school :: Maybe Text
  , player_id :: Int
  } deriving (Show)

instance FromJSON Player where
  parseJSON = withArray "Player" $ \v -> Player
    <$> parseJSON (v ! 3)
    <*> parseIntFromText "Number" (v ! 4)
    <*> parseJSON (v ! 5)
    <*> parseJSON (v ! 6)
    <*> parseIntFromText "Weight" (v ! 7)
    <*> parseJSON (v ! 8)
    <*> parseJSON (v ! 9)
    <*> parseJSON (v ! 10)
    <*> parseJSON (v ! 11)
    <*> parseJSON (v ! 12)
    where
      parseIntFromText :: String -> Value -> Parser Int
      parseIntFromText s = withText s (return . read . Text.unpack)

data Height = Height
  { feet :: Int
  , inches :: Int
  } deriving (Show)

instance FromJSON Height where
  parseJSON = withText "Height" $ \t -> return $ case Text.splitOn "-" t of
    (x1:x2:[]) -> Height 
      { feet = read (Text.unpack x1)
      , inches = read (Text.unpack x2) 
      }
    _ -> error "Invalid height"

data Experience 
  = Rookie
  | Vet Int
  deriving (Show)

instance FromJSON Experience where
  parseJSON = withText "Experience" $ \t -> return $ case t of
    "R" -> Rookie
    _ -> Vet $ read (Text.unpack t)

data Position 
  = Guard
  | Forward
  | Center
  | Multi Position Position
  | Unknown

instance Show Position where
  show Guard = "G"
  show Forward = "F"
  show Center = "C"
  show (Multi p1 p2) = show p1 <> "-" <> show p2

instance FromJSON Position where
  parseJSON = withText "Position" $ return . fromMaybe Unknown . posFromText
    where 
      posFromText :: Text -> Maybe Position
      posFromText "G" = Just Guard
      posFromText "F" = Just Forward
      posFromText "C" = Just Center
      posFromText pt = case Text.splitOn "-" pt of 
        (x1:x2:[]) -> Multi <$> (posFromText x1) <*> (posFromText x2)
        _ -> Nothing
