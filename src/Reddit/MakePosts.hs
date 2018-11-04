{-# LANGUAGE OverloadedStrings #-}
module Reddit.MakePosts where

import Prelude
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Reddit.Actions.Post as Reddit
import qualified Reddit.Types.Post as Reddit
import Reddit.Types (RedditT, SubredditName)
import Util

mkPost
  :: MonadIO m
  => SubredditName
  -> Reddit.Post
  -> [Reddit.Post]
  -> RedditT m Reddit.PostID
mkPost srName gameThread highlights = Reddit.submitSelfPost srName title body
  where
    title = newThreadTitle gameThread
    threadBody = mkLinkMarkdown (Reddit.permalink gameThread) "Game Thread"
    -- Reddit markdown needs a blank line between each entry to actually render
    -- a new line, so append 2 new lines in between each entry
    body = Text.intercalate "\n\n" $ threadBody : map formatHighlightForBody highlights

newThreadTitle :: Reddit.Post -> Text
newThreadTitle post = Text.strip $ fromMaybe title $ Text.stripPrefix prefix title
  where
    title = Reddit.title post
    prefix = "GAME THREAD:"

-- |
-- Maybe TODO: Pass around links earlier in the pipeline
--
formatHighlightForBody :: Reddit.Post -> Text
formatHighlightForBody highlightPost = case Reddit.content highlightPost of
  Reddit.Link t -> mkLinkMarkdown t title <> " " <> mkLinkMarkdown permalink "Comments"
  _ -> mkLinkMarkdown permalink title
  where
    permalink = Reddit.permalink highlightPost
    title = Reddit.title highlightPost

mkLinkMarkdown :: Text -> Text -> Text
mkLinkMarkdown url display = Text.concat [ "[", display, "]", "(", url, ")" ]
