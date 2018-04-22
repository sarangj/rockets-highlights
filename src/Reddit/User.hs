{-# LANGUAGE OverloadedStrings #-}
module Reddit.User (run) where

import Prelude
import Control.Monad.IO.Class
import Data.Configurator
import Data.Configurator.Types
import Data.Default
import Reddit

run :: RedditT IO a -> IO (Either (APIError RedditError) a)
run action = do
  opts <- options
  runRedditWith opts action
  where
    options = do
      conf <- config
      user <- require conf "user"
      pass <- require conf "pass"
      pure $ def
        { loginMethod = Credentials user pass
        , customUserAgent = Just "github.com/sarangj"
        }

config :: IO Config
config = load [Required "$(CONFIGS)/reddit-signin.cfg"]
