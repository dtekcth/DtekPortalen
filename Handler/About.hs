{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.About where

import Import

getAboutR :: Handler RepHtml
getAboutR = do
    standardLayout $ do
        setDtekTitle "Om portalen"
        addWidget $(widgetFile "about")
