{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Posts where

import Import
import qualified Data.Text as T
import Control.Applicative ((<*>), (<$>))
import Yesod.Paginator
import Yesod.Markdown
import Data.Shorten
import Helpers.Post

getPostsR :: Handler RepHtml
getPostsR = do
    posts <- runDB $ selectList [] [Desc PostCreated]
    (posts', widget :: Widget) <- paginate 5 (map (postSlug . snd) posts)
    standardLayout $ do
        setDtekTitle "Gamla inlägg"
        addWidget widget

getPostR :: Text -> Handler RepHtml
getPostR slug = do
    standardLayout $ do
        setDtekTitle "Specifikt inlägg"
        addWidget $ slugToPostWidget True slug

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

deleteDelPostR :: Text -> Handler RepHtml
deleteDelPostR slug = do
    p <- runDB $ getBy $ UniquePost slug
    case p of
        Just (key, _) -> do
            runDB $ delete key
            setSuccessMessage "Inlägg raderat!"
        Nothing -> setErrorMessage "Inlägg ej funnet."
    redirect RedirectTemporary ManagePostsR
