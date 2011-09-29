{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Contact where

import Foundation
import StandardLayout

getContactR :: Handler RepHtml
getContactR = do
    standardLayout $ do
        setDtekTitle "Kontakt"
        addWidget $(widgetFile "contact")
