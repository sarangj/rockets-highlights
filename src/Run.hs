{-# LANGUAGE OverloadedStrings #-}
module Run
  ( ActionType(..)
  , run
  ) where

import qualified Reddit.Config as Config
import qualified Reddit.RocketsHighlight as RocketsHighlight
import Reddit.MakePosts
import qualified Reddit.Types.Post as Reddit
import qualified Reddit.Types.Subreddit as Subreddit
import qualified Reddit.User as User
import Util (ask, Answer(..))

data ActionType = DryRun | XPost

run :: ActionType -> IO ()
run actionType = do
  postsOrError <- User.run RocketsHighlight.findAll
  case postsOrError of
    Left error -> putStrLn $ show error
    Right posts -> runImpl actionType posts

runImpl :: ActionType -> [Reddit.Post] -> IO ()
runImpl _ [] = putStrLn "No posts found!"
runImpl DryRun ps = mapM_ displayPost ps
runImpl XPost ps = do
  putStrLn "Posts:"
  mapM_ displayPost ps 
  answer <- ask "Look good?"
  case answer of
    Yes -> submitPosts ps
    No -> putStrLn "aborted"

submitPosts :: [Reddit.Post] -> IO ()
submitPosts ps = do
  targetSR <- Config.targetSR
  postResult <- User.run $ copyAndSubmitPosts targetSR ps
  case postResult of
    Left apiError -> putStrLn $ show apiError
    Right res -> mapM_ (displayResult . snd) res

displayPost :: Reddit.Post -> IO ()
displayPost = putStrLn . show . mappend "reddit.com" . Reddit.permalink
