{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Links where

import Foundation
import StandardLayout

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getLinksR :: Handler RepHtml
getLinksR = standardLayout $ do
    setDtekTitle "Länksamling"
    addWidget $(widgetFile "links")

