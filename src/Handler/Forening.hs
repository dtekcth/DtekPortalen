{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Forening where

import Import

getForeningR :: Forening -> Handler Html
getForeningR forening = standardLayout $(widgetFile "forening")
