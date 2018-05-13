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

data ActionType = DryRun | XPost

run :: ActionType -> IO ()
run actionType = do
  postsOrError <- User.run RocketsHighlight.findAll
  case postsOrError of
    Left error -> putStrLn $ show error
    Right posts -> runImpl actionType posts

runImpl :: ActionType -> [Reddit.Post] -> IO ()
runImpl DryRun = mapM_ (putStrLn . show . mappend "reddit.com" . Reddit.permalink)
runImpl XPost = \posts -> do
  targetSR <- Config.targetSR
  _ <- User.run $ copyAndSubmitPosts targetSR posts
  pure ()
