module Util where

import Prelude

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

mapPairsM :: Monad m => (b -> m b') -> [(a, b)] -> m [(a, b')]
mapPairsM f = mapM (\(a, b) -> (,) a <$> f b)

putTextLn :: Text -> IO ()
putTextLn = putStrLn . Text.unpack

ask :: String -> IO Answer 
ask q = do
  putStrLn $ q <> " (y/n)"
  answerStr <- getLine
  case answerStr of
    "y" -> pure Yes
    "n" -> pure No
    _ -> ask q

data Answer = Yes | No
