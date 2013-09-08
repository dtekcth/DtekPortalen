{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Admin where

import Import

getAdminR :: Handler Html
getAdminR = do
    u <- fmap entityVal requireAuth
    fs <- liftIO $ getMemberships u
    let okRoutes = filter (maybe False (any (`elem` fs)) . routePrivileges) adminRoutes
    standardLayout $ do
        setDtekTitle "Administrera portalen"
        $(widgetFile "admin")
