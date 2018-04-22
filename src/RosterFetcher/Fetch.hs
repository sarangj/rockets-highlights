{-# LANGUAGE OverloadedStrings #-}
module RosterFetcher.Fetch (rocketsRoster) where

import Prelude
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Vector as Vector
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as C8
import RosterFetcher.Parser
import RosterFetcher.Types

rocketsRoster :: MonadIO m => m (Either Text [Text])
rocketsRoster = parseResponse <$> httpLBS rocketsRequest

parseResponse :: Response ByteString -> Either Text [Text]
parseResponse resp = case HashMap.lookup "resultSets" =<< rawData of 
  Just (Array v) -> case Vector.head v of
    Object o -> Right $ map player_name $ parsePlayers o
    _ -> Left "Failed to decode response"
  _ -> Left "Failed to decode response"
  where
    rawData :: Maybe Object
    rawData = (decode . getResponseBody) resp

rocketsRequest :: Request
rocketsRequest = setRequestHeaders headers 
  $ setTeamID rocketsID
  $ "GET http://stats.nba.com/stats/commonteamroster"
  where
    rocketsID = 1610612745

setTeamID :: Int -> Request -> Request
setTeamID n = setRequestQueryString 
  [ ("TeamID", Just teamIDBS)
  , ("Season", Just "2017-18") 
  ]
  where
    teamIDBS = C8.pack (show n)

headers :: [(HeaderName, C8.ByteString)]
headers = 
  [ ("accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8")
  , ("user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_2_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36")
  , ("Dnt", "1")
  , ("Accept-Encoding", "gzip, deflate")
  , ("Accept-Language", "en-US,en;q=0.9")
  ]

