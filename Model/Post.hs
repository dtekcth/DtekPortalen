module Model.Post where

import Prelude
import Model.Persist
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Monoid (mconcat)
import Yesod.Markdown

postContent :: Post -> Markdown
postContent post = mconcat $ [postTeaser post | postSumem post] ++ [postBody post]

postIsEdited :: Post -> Bool
postIsEdited post = postCreated post /= postEdited post
