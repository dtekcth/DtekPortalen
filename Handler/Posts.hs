{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Handler.Posts where

import Import
import Yesod.Auth (requireAuthId)
import Helpers.Post
import Helpers.Shorten

getPostsR :: Handler Html
getPostsR = do
    (map entityVal -> posts, widget) <- runDB $ selectList [] [Desc PostCreated]
    standardLayout $ do
        setDtekTitle "Gamla inlägg"
        $(widgetFile "posts")

getPostR :: Text -> Handler Html
getPostR slug = standardLayout $ do
        setDtekTitle "Specifikt inlägg"
        slugToPostWidget True slug

postPostR :: Text -> Handler Html
postPostR = getPostR

getManagePostsR :: Handler Html
getManagePostsR = do
    uid <- requireAuthId
    (map entityVal -> posts) <- runDB $ selectList [] [Desc PostCreated]
    defaultLayout $ do
        setDtekTitle "Administrera inlägg"
        let (postslist :: Widget) = $(widgetFile "postslist")
        $(widgetFile "manage")

postManagePostsR :: Handler Html
postManagePostsR = getManagePostsR

getEditPostR :: Text -> Handler Html
getEditPostR slug = do
    uid <- requireAuthId
    mkpost <- runDB $ selectFirst [PostSlug ==. slug] []
    defaultLayout $ do
        setDtekTitle "Redigera inlägg"
        $(widgetFile "editpost")

postEditPostR :: Text -> Handler Html
postEditPostR = getEditPostR

getDelPostR :: Text -> Handler Html
getDelPostR slug = do
    p <- runDB $ getBy $ UniquePost slug
    case p of
        Just (entityKey -> key) -> do
            runDB $ delete key
            setSuccessMessage "Inlägg raderat!"
        Nothing -> setErrorMessage "Inlägg ej funnet."
    redirect ManagePostsR
