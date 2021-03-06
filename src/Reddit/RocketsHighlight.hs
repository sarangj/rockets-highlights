{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reddit.RocketsHighlight
  ( doSearch
  , SearchResult(..)
  ) where

import Prelude
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import qualified Reddit.Actions.Search as RedditSearch
import qualified Reddit.Constants as Constants
import qualified Reddit.Types.Post as Reddit
import qualified Reddit.Types.Listing as Reddit
import qualified Reddit.Types.SearchOptions as RedditSearch
import qualified Reddit.Types.Subreddit as Subreddit
import Reddit.Types.Reddit (RedditT)
import Reddit
import RosterFetcher.Fetch

data SearchResult
  = HighlightsFor Reddit.Post [Reddit.Post]
  | Empty

doSearch :: MonadIO m => RedditT m SearchResult
doSearch = do
  gameThread <- latestLikelyGameThread
  case gameThread of
    Nothing -> pure Empty
    Just gt -> HighlightsFor gt <$> findAfter (Reddit.postID gt)

findAfter
  :: MonadIO m
  => Reddit.PostID
  -> RedditT m [Reddit.Post]
findAfter postID = do
  posts <- Reddit.contents <$> Reddit.getPosts' options Reddit.New sr
  case posts of
    [] -> pure []
    (p:_) -> mappend (filter likelyRocketsHighlight posts) <$> findAfter (Reddit.postID p)
    where
      options = Options
        { pagination = Just (Before postID)
        , limit = Just 100
        }
      sr = Just Constants.nbaSubreddit

latestLikelyGameThread :: Monad m => RedditT m (Maybe Reddit.Post)
latestLikelyGameThread =
  firstCandidate <$> RedditSearch.search sr def RedditSearch.New search
  where
    search = "Game Thread Rockets"
    sr = Just Constants.nbaSubreddit
    firstCandidate :: PostListing -> Maybe Post
    firstCandidate = find (Text.isInfixOf "GAME THREAD" . Reddit.title) . Reddit.contents

likelyRocketsHighlight :: Reddit.Post -> Bool
likelyRocketsHighlight p = (isStreamable p || isHighlight p)
  && (titleLikelyRocketsRelated $ Text.toLower (Reddit.title p))

titleLikelyRocketsRelated :: Text -> Bool
titleLikelyRocketsRelated title =
  Text.isInfixOf "rockets" title
  || titleHasNameMatch title

titleHasNameMatch :: Text -> Bool
titleHasNameMatch title = any (\n -> Text.isInfixOf n title) matchableNames

isStreamable :: Reddit.Post -> Bool
isStreamable = (=="streamable.com") . Reddit.domain

isHighlight :: Reddit.Post -> Bool
isHighlight = (== (Just "highlights")) . Reddit.flairClass
-- |
-- I *could* autogenerate the roster using `RosterFetcher.fetch`, but some names
-- require full name matching (e.g. Chris Paul) while others do not (Harden).
-- The below are hardcoded based on intution.
-- We can still use RosterFetcher.fetch to help update this in the future,
-- though I may just make that a standalone library.
matchableNames :: [Text]
matchableNames =
  [ "austin rivers"
  , "chriss"
  , "carter-williams"
  , "brandon knight"
  , "chris paul"
  , "cp3"
  , "danuel house"
  , "gary clark"
  , "melo"
  , "ennis"
  , "zhou"
  , "qi"
  , "eric gordon"
  , "vince edwards"
  , "vincent edwards"
  , "harden"
  , "gerald green"
  , "capela"
  , "pj tucker"
  , "nene"
  , "hartenstein"
  ]
