{-# LANGUAGE OverloadedStrings #-}
module Config where

import Prelude
import Data.Maybe
import Data.Object
import Yesod.Default.Config
import Data.Text (Text, unpack)

-- | So far only the Google Calendar Url.
type Extra = String

loadExtra :: DefaultEnv -> TextObject -> IO Extra
loadExtra _ to = do
    let m = fromMaybe (fail "Expected map") $ fromMapping to
    let murl = lookupScalar "calendar" (m :: [(Text, TextObject)])
    return $ unpack $ fromMaybe (error "no calendar url") (murl :: Maybe Text)

