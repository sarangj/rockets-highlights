{-# LANGUAGE OverloadedStrings #-}
module Reddit.User where

import Prelude
import Control.Monad.IO.Class
import Data.Default
import Reddit
import qualified Reddit.Config as Config

run :: RedditT IO a -> IO (Either (APIError RedditError) a)
run action = do
  opts <- options
  runRedditWith opts action
  where
    options = do
      user <- Config.user
      pass <- Config.pass
      pure $ def
        { loginMethod = Credentials user pass
        , customUserAgent = Just "github.com/sarangj"
        }

handleResult :: Show a => Either (APIError RedditError) a -> IO ()
handleResult (Left err) = putStrLn $ show err
handleResult (Right r) = putStrLn $ show r
