{-# LANGUAGE OverloadedStrings #-}
module Reddit.Constants where

import Reddit.Types.Subreddit (SubredditName)
import qualified Reddit.Types.Subreddit as Subreddit

nbaSubreddit :: SubredditName
nbaSubreddit = Subreddit.R "nba"
