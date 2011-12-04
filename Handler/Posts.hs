{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Posts where

import Foundation
import StandardLayout
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ((<*>), (<$>))
import Yesod.Goodies.Paginate
import Yesod.Goodies.Shorten
import Yesod.Goodies.Markdown
import Helpers.Post

getPostsR :: Handler RepHtml
getPostsR = do
    let po = PageOptions {
        itemsPerPage = 4
      , showItems    = mapM_ (slugToPostWidget True)
    }
    posts <- runDB $ selectList [] [Desc PostCreated]
    standardLayout $ do
        setDtekTitle "Gamla inlägg"
        addWidget $ paginate po (map (postSlug . snd) posts)

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
        let postslist = $(widgetFile "postslist")
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
