{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Forening where

import Import

getForeningR :: Forening -> Handler RepHtml
getForeningR forening = standardLayout $(widgetFile "forening")
