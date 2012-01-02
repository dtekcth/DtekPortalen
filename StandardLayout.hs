{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StandardLayout (standardLayout)
  where

import Prelude
import Foundation
import Data.Time
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import Scrapers.CalendarFeed (EventInfo(..))
import System.Locale
import Data.IORef
import Data.Shorten(shorten)
import Yesod.Markdown (markdownToHtml)

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
        sndmd <- documentFromDB "sndfrontpage"
        return $(widgetFile "lmenu" )
    mkrmenu  = do
        (Dtek _ _ _ _ (CachedValues einsteinRef calendarRef)) <- getYesod
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
        return $ $(widgetFile "rmenu" )
    niceShowEvent :: EventInfo -> String
    niceShowEvent event =
      let format = formatTime defaultTimeLocale
      in  (title event) ++ ": " ++ format "%A %R" (startTime event) ++ "-"
       ++ format "%R" (endTime event)
