{-# LANGUAGE OverloadedStrings #-}
module Reddit.MakePosts where

import Prelude
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Reddit.Actions.Post as Reddit
import qualified Reddit.Types.Post as Reddit
import Reddit.Types (RedditT, SubredditName)
import Util

type SubmissionResult = Either Text Reddit.PostID 

copyAndSubmitPosts 
  :: MonadIO m
  => SubredditName 
  -> [Reddit.Post] 
  -> RedditT m [(Reddit.PostID, SubmissionResult)] 
copyAndSubmitPosts srName = mapM toResultPair
  where 
    toResultPair p = (,) (Reddit.postID p) <$> mkLinkXPost srName p

mkLinkXPost 
  :: MonadIO m 
  => SubredditName
  -> Reddit.Post 
  -> RedditT m SubmissionResult
mkLinkXPost srName post = case Reddit.content post of 
  Reddit.Link t -> do
    pID <- Reddit.submitLink srName (Reddit.title post) t  
    pure (Right pID)
  c -> pure . Left $ "Expected a link post, got a " <> (Text.pack (show c)) 

displayResult :: SubmissionResult -> IO ()
displayResult (Left err) = putTextLn err
displayResult (Right postID) = putStrLn $ "Submitted " <> (show postID)
