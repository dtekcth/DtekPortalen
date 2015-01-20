{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Handler.Root where

import Import
import Helpers.Post

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler Html
getRootR = do
    (map entityVal -> posts) <- runDB $ selectList [] [LimitTo 5, Desc PostCreated]
    standardLayout $ do
        setDtekTitle "Startsida"
        $(widgetFile "homepage")
