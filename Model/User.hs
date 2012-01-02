module Model.User where

import Model.Persist
import Data.Maybe (fromMaybe)
import Data.Text (Text)

userCalcName :: User -> Text
userCalcName u = fromMaybe (userIdent u) (userNick u)
