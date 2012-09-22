{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module StandardLayout (standardLayout)
  where

import Prelude
import Foundation
import Data.Time
import qualified Data.Text as T
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import Scrapers.CalendarFeed (EventInfo(..))
import System.Locale
import Data.IORef
import Text.Markdown (Markdown(..), markdown, def)
import Text.Blaze (ToMarkup (toMarkup))
import Yesod (Yesod (defaultLayout))
import Yesod

standardLayout :: Widget -> Handler RepHtml
standardLayout contentWidget = do
    mu <- maybeAuth
    (rmenu :: Widget)  <- mkrmenu
    (lmenu :: Widget)  <- mklmenu
    (header :: Widget) <- mkHeader mu
    defaultLayout $ addWidget $(widgetFile "standard")
  where
    (footer :: Widget) = $(widgetFile "footer")
    mkHeader mu = return $(widgetFile "header")
    mklmenu  = do
        sndmd <- documentFromDB "stat_sndfrontpage"
        dagmd@(Markdown dagtext) <- documentFromDB "stat_dagads"
        let dagmdEmtpy = dagtext == ""
        return $(widgetFile "lmenu" )
    mkrmenu  = do
        (cache -> (CachedValues einsteinRef calendarRef)) <- getYesod
        einsteinScrapResult <- liftIO $ readIORef einsteinRef
        eventInfos          <- liftIO $ readIORef calendarRef
        esMenu <- case einsteinScrapResult of
             Nothing            -> return ["Ingen meny tillgänglig"]
             Just (esWeek, sss) -> do
                day <- liftIO $ fmap utctDay getCurrentTime
                let (week, weekday) = mondayStartWeek day
                return $ head $
                    [["Stängt under helgdag"] | weekday `notElem` [1..5]]
                 ++ [["Einstein har ej uppdaterat veckan"] | esWeek /= week]
                 ++ [sss !! (weekday - 1)]
        return $(widgetFile "rmenu" )
    niceShowEvent :: EventInfo -> String
    niceShowEvent event =
      let format = formatTime defaultTimeLocale
      in  (title event) ++ ": " ++ format "%A %R" (startTime event) ++ "-"
       ++ format "%R" (endTime event)
