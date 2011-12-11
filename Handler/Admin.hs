{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Admin where

import Foundation
import StandardLayout

getAdminR :: Handler RepHtml
getAdminR = do
    (_, u) <- requireAuth
    fs <- liftIO $ getMemberships u
    let okRoutes = filter (maybe False (any (`elem` fs)) . routePrivileges) adminRoutes
    standardLayout $ do
        setDtekTitle "Administrera portalen"
        addWidget $(widgetFile "admin")
