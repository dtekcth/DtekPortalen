{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.About where

import Foundation
import StandardLayout

getAboutR :: Handler RepHtml
getAboutR = do
    standardLayout $ do
        setDtekTitle "Om portalen"
        addWidget $(widgetFile "about")
