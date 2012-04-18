module Model.Post where

import Prelude
import Model.Persist
import Data.Text (Text, strip, pack)
import Data.Monoid (mconcat)
import Yesod.Markdown

-- This should ideally be provided by markdown but isn't
unMarkdown :: Markdown -> Text
unMarkdown (Markdown s) = pack s

postContent :: Post -> Markdown
postContent post = mconcat $ [postTeaser post | postSumem post] ++ [postBody post]

postIsEdited :: Post -> Bool
postIsEdited post = postCreated post /= postEdited post

moreToRead :: Post -> Bool
moreToRead post = let f = strip . unMarkdown . ($ post)
                  in  f postTeaser /= f postContent
