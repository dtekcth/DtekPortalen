{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Handler.Posts where

import Import
import Yesod.Paginator
import Yesod.Markdown
import Data.Shorten
import Helpers.Post

getPostsR :: Handler RepHtml
getPostsR = do
    (map snd -> posts, widget :: Widget) <- selectPaginated 10 [] [Desc PostCreated]
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
    (uid, _) <- requireAuth
    posts <- runDB $ selectList [] [Desc PostCreated]
    defaultLayout $ do
        setDtekTitle "Administrera inlägg"
        let (postslist :: Widget) = $(widgetFile "postslist")
        addWidget $(widgetFile "manage")

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

getEditPostR :: Text -> Handler RepHtml
getEditPostR slug = do
    (uid, _) <- requireAuth
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
        Just (key, _) -> do
            runDB $ delete key
            setSuccessMessage "Inlägg raderat!"
        Nothing -> setErrorMessage "Inlägg ej funnet."
    redirect RedirectTemporary ManagePostsR
