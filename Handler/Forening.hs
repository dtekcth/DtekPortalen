{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Forening where

import Foundation
import StandardLayout
import Data.Text (Text)

getForeningR :: Forening -> Handler RepHtml
getForeningR forening = standardLayout $ addWidget $(widgetFile "forening") 

