{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Admin where

import Foundation
import StandardLayout

getAdminR :: Handler RepHtml
getAdminR = do
    standardLayout $ do
        setDtekTitle "Administrera portalen"
        addWidget $(widgetFile "admin")
