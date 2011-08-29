{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module StandardLayout (standardLayout) where

import Foundation
import Data.Time
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import Einstein

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
        einsteinScrapResult <- liftIO scrapEinstein 
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

