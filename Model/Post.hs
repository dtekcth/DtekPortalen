module Model.Post where

import Prelude
import Model.Persist
import Data.Text (Text, strip, pack)
import Data.Monoid (mconcat)
import Yesod.Markdown

-- This should ideally be provided by markdown but isn't
unMarkdown :: Markdown -> Text
unMarkdown (Markdown s) = pack s

-- | The "full" content of the article, including the teaser if concat
postContent :: Post -> Markdown
postContent post = mconcat $ [postTeaser post | postSumem post] ++ [postBody post]

-- | Yes if it's ever been edited
postIsEdited :: Post -> Bool
postIsEdited post = postCreated post /= postEdited post

-- | Yes if the the content of the teaser is less than the content
-- of whole article.
moreToRead :: Post -> Bool
moreToRead post = let f = strip . unMarkdown . ($ post)
                  in  f postTeaser /= f postContent
