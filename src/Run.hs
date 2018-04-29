module Run where

import qualified Reddit.RocketsHighlight as RocketsHighlight
import qualified Reddit.Types.Post as Reddit
import qualified Reddit.User as User

run :: IO ()
run = do
  postsOrError <- User.run $ RocketsHighlight.findAll
  putStrLn $ case postsOrError of
    Left error -> show error
    Right posts -> show $ map (mappend "reddit.com" . Reddit.permalink) posts
