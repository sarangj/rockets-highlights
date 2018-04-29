{-# LANGUAGE OverloadedStrings #-}
module Run where

import qualified Reddit.RocketsHighlight as RocketsHighlight
import qualified Reddit.Types.Post as Reddit
import qualified Reddit.User as User

run :: IO ()
run = do
  postsOrError <- User.run $ RocketsHighlight.findAll
  case postsOrError of
    Left error -> putStrLn $ show error
    Right posts -> mapM_ (putStrLn . show . mappend "reddit.com" . Reddit.permalink) posts
