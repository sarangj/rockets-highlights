{-# LANGUAGE OverloadedStrings #-}
module Reddit.Config
  ( pass
  , targetSR
  , user
  ) where

import Prelude
import Data.Configurator
import Data.Configurator.Types
import Data.Text (Text)
import Reddit.Types.Subreddit (SubredditName)
import qualified Reddit.Types.Subreddit as Subreddit

-- |
-- This module is kinda bad because you have to reload the config each time
-- but meh.  Perhaps a good exercise for using Reader / ReaderT
--

pass :: IO Text
pass = require' "pass" =<< config

user :: IO Text
user = require' "user" =<< config

targetSR :: IO SubredditName
targetSR = Subreddit.R <$> (require' "sr" =<< config)

require' :: Configured a => Text -> Config -> IO a
require' = flip require

config :: IO Config
config = load [Required "$(CONFIGS)/reddit-signin.cfg"]
