{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Handler.Posts where

import Import
import Yesod.Auth (requireAuthId)
import Text.Markdown
import Helpers.Post
import Yesod.Paginator (selectPaginated)
import Data.Shorten

getPostsR :: Handler RepHtml
getPostsR = do
    (map entityVal -> posts, widget) <- runDB $ selectPaginated 10 [] [Desc PostCreated]
    standardLayout $ do
        setDtekTitle "Gamla inlägg"
        addWidget $(widgetFile "posts")

getPostR :: Text -> Handler RepHtml
getPostR slug = do
    standardLayout $ do
        setDtekTitle "Specifikt inlägg"
        addWidget $ slugToPostWidget True slug

postPostR :: Text -> Handler RepHtml
postPostR = getPostR

getManagePostsR :: Handler RepHtml
getManagePostsR = do
    uid <- requireAuthId
    (map entityVal -> posts) <- runDB $ selectList [] [Desc PostCreated]
    defaultLayout $ do
        setDtekTitle "Administrera inlägg"
        let (postslist :: Widget) = $(widgetFile "postslist")
        addWidget $(widgetFile "manage")

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

getEditPostR :: Text -> Handler RepHtml
getEditPostR slug = do
    uid <- requireAuthId
    mkpost <- runDB $ selectFirst [PostSlug ==. slug] []
    defaultLayout $ do
        setDtekTitle "Redigera inlägg"
        addWidget $(widgetFile "editpost")

postEditPostR :: Text -> Handler RepHtml
postEditPostR = getEditPostR

getDelPostR :: Text -> Handler RepHtml
getDelPostR slug = do
    p <- runDB $ getBy $ UniquePost slug
    case p of
        Just (entityKey -> key) -> do
            runDB $ delete key
            setSuccessMessage "Inlägg raderat!"
        Nothing -> setErrorMessage "Inlägg ej funnet."
    redirect ManagePostsR
