{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module StandardLayout
   -- (standardLayout)
   -- FIXME Rename file and export requireFunctions
  where

import Foundation
import Data.Time
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import CalendarFeed (EventInfo(..))
import System.Locale
import Data.IORef
import Yesod.Goodies(shorten)
import qualified Data.Text as T

standardLayout contentWidget = do
    mu <- maybeAuth
    rmenu <- mkrmenu
    header <- mkHeader mu
    defaultLayout $ addWidget $(widgetFile "standard")
  where
    footer = $(widgetFile "footer")
    mkHeader mu = return $(widgetFile "header")
    lmenu  = $(widgetFile "lmenu" )
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

requireMemberships :: [Forening] -> Handler (UserId, User)
requireMemberships fs = do
    let fs' = Webredax : fs
    tup@(key, u) <- requireAuth
    emember <- liftIO $ checkMemberships fs' u
    case emember of
        Left errorMsg -> do
            setErrorMessage $ "Internal error: " `mappend` toHtml errorMsg
            redirect RedirectTemporary RootR
        Right False   -> permissionDenied $
            "Åtkomst nekad, du måste tillhöra någon av " `mappend` T.pack (show fs')
        _             -> return tup

requireEditor :: Handler (UserId, User)
requireEditor = requireMemberships [Styret, DAG]
