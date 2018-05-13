module Util where

import Prelude

import Data.Text (Text)
import qualified Data.Text as Text

mapPairsM :: Monad m => (b -> m b') -> [(a, b)] -> m [(a, b')]
mapPairsM f = mapM (\(a, b) -> (,) a <$> f b)

putTextLn :: Text -> IO ()
putTextLn = putStrLn . Text.unpack
