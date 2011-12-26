{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Contact where

import Import

getContactR :: Handler RepHtml
getContactR = do
    standardLayout $ do
        setDtekTitle "Kontakt"
        addWidget $(widgetFile "contact")
