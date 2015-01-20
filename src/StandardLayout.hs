{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module StandardLayout (standardLayout)
  where

import Prelude
import Foundation
import Data.Time
import Scrapers.CalendarFeed (EventInfo(..))
import Data.IORef
import Text.Markdown (Markdown(..))
import Text.Blaze (ToMarkup (toMarkup))
import Yesod
import Settings

standardLayout :: Widget -> Handler Html
standardLayout contentWidget = do
    mu <- maybeAuth
    (rmenu :: Widget)  <- mkrmenu
    (lmenu :: Widget)  <- mklmenu
    (header :: Widget) <- mkHeader mu
    defaultLayout $(widgetFile "standard")
  where
    (footer :: Widget) = $(widgetFile "footer")
    mkHeader mu = return $(widgetFile "header")
    mklmenu  = do
        sndmd <- documentFromDB "stat_sndfrontpage"
        dagmd@(Markdown dagtext) <- documentFromDB "stat_dagads"
        let dagmdEmtpy = dagtext == ""
        return $(widgetFile "lmenu" )
    mkrmenu  = do
        (cache -> (CachedValues calendarRef)) <- getYesod
        eventInfos          <- liftIO $ readIORef calendarRef
        return $(widgetFile "rmenu" )
    niceShowEvent :: EventInfo -> String
    niceShowEvent event =
      let format = formatTime swedishTimeLocale
      in  (title event) ++ ": " ++ format "%A %R" (startTime event) ++ "-"
       ++ format "%R" (endTime event)
