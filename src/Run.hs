{-# LANGUAGE OverloadedStrings #-}
module Run
  ( ActionType(..)
  , run
  ) where

import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Reddit.Config as Config
import Reddit.RocketsHighlight (SearchResult(..))
import qualified Reddit.RocketsHighlight as RocketsHighlight
import Reddit.MakePosts
import qualified Reddit.Types.Post as Reddit
import qualified Reddit.Types.Subreddit as Subreddit
import qualified Reddit.User as User
import Util (ask, Answer(..))

data ActionType = DryRun | XPost

run :: ActionType -> IO ()
run actionType = do
  searchResultOrError <- User.run RocketsHighlight.doSearch
  putStrLn "Did search"
  case searchResultOrError of
    Left error -> putStrLn $ show error
    Right res -> runImpl actionType res

runImpl :: ActionType -> SearchResult -> IO ()
runImpl DryRun (HighlightsFor _ highlights) = mapM_ displayPost highlights
runImpl _ Empty = putStrLn "No Game Thread Found!"
runImpl XPost (HighlightsFor _ []) = putStrLn "No Posts Found!"
runImpl XPost (HighlightsFor gameThread highlights) = do
  putStrLn "Posts:"
  mapM_ displayPost highlights
  answer <- ask "Look good?"
  case answer of
    Yes -> submitPosts gameThread highlights
    No -> putStrLn "aborted"

submitPosts :: Reddit.Post -> [Reddit.Post] -> IO ()
submitPosts gameThread highlights = do
  targetSR <- Config.targetSR
  postResult <- User.run $ mkPost targetSR gameThread highlights
  case postResult of
    Left apiError -> putStrLn $ show apiError
    Right postID -> putStrLn $ "Submitted " <> (postLink postID)

displayPost :: Reddit.Post -> IO ()
displayPost = putStrLn . show . mappend "reddit.com" . Reddit.permalink

postLink :: Reddit.PostID -> String
postLink (Reddit.PostID p) = "reddit.com/r/sarangscoolstuff/comments/" <> (Text.unpack p)
