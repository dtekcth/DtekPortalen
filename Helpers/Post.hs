{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Helpers.Post where

import Foundation
import Control.Applicative ((<$>), (<*>))
import Data.Time (getCurrentTime)
import Network.URL
import Text.Hamlet(shamlet)
import Data.Maybe (fromMaybe)
import Yesod.Goodies.Markdown
import Yesod.Goodies.Shorten
import Yesod.Goodies.Time
import Data.Text (Text)
import qualified Data.Text as T

slugToPostWidget :: Bool -- | Full?
                 -> Text -- | The slug
                 -> Widget
slugToPostWidget isFull slug = do
    mpost <- lift $ runDB $ selectFirst [PostSlug ==. slug] []
    case mpost of
        Just (_key, post) -> do postToWidget isFull post
        Nothing   -> addHtml [shamlet|Posten med slug "#{slug}" hittades inte!|]

postToWidget :: Bool -- | Full?
             -> Post
             -> Widget
postToWidget isFull post = do
    creator <- fmap safeExtract $ lift $ runDB $ get (postCreator post)
    editor <- fmap safeExtract  $ lift $ runDB $ get (postEditor post)
    prettyCreated <- lift $ humanReadableTime $ postCreated post
    prettyEdited  <- lift $ humanReadableTime $ postEdited post
    addWidget $ if isFull then $(widgetFile "fullpost") else $(widgetFile "teasepost")
  where
    safeExtract = fromMaybe "(borttagen)" . fmap userCalcName

data PostEditForm = PostEditForm
    { formSlug   :: Text
    , formTitle  :: Text
    , formTeaser :: Markdown
    , formBody   :: Markdown
    , formSumem  :: Bool
    }

runPostForm :: Maybe (PostId, Post) -> UserId -> Widget
runPostForm mkpost uid = do
    ((res, form), enctype) <- lift $ runFormPost $ postForm $ fmap snd mkpost
    isPreview <- lift $ runInputPost $ iopt boolField "preview"
    case res of
        FormSuccess pf ->
            if isPreview /= Just True
                then lift $ processFormResult pf
                else do post <- lift $ postFromForm pf
                        [whamlet| #{markdownToHtml $ postContent post} |]
        _ -> return ()

    [whamlet|
        <div .post_input>
            <form enctype="#{enctype}" method="post">
                <table>
                    ^{form}
                    <tr>
                        <td>&nbsp;
                        <td .buttons>
                            <input type="submit" name="Förhandsgranskning" value="yes">
                            $maybe _ <- mkpost
                                <input type="submit" value="Updatera">
                            $nothing
                                <input type="submit" value="Skapa">

        |]

    where
        processFormResult :: PostEditForm -> Handler ()
        processFormResult pf = do
            p <- postFromForm pf
            case mkpost of
                Just (k, _) -> do  -- We are going to replace
                    updatePost k p
                    setSuccessMessage "Inlägg uppdaterat!"
                _           -> do --  Post should be inserted
                    _ <- runDB $ insertBy p
                    setSuccessMessage "Inlägg skapat!"
            redirect RedirectTemporary ManagePostsR

        postFromForm :: PostEditForm -> Handler Post
        postFromForm pf = do
            now <- liftIO getCurrentTime
            return Post
                { postSlug        = formSlug pf
                , postTitle       = formTitle pf
                , postTeaser      = formTeaser pf
                , postBody        = formBody pf
                , postSumem       = formSumem pf
                , postCreator     = uid
                , postCreated     = now
                , postEditor      = uid
                , postEdited      = now
                }

        updatePost :: PostId -> Post -> Handler ()
        updatePost key new = do
            now <- liftIO getCurrentTime
            runDB $ update key
                [ PostSlug       =. postSlug new
                , PostTitle      =. postTitle new
                , PostTeaser     =. postTeaser new
                , PostBody       =. postBody new
                , PostSumem      =. postSumem new
                , PostEditor     =. uid
                , PostEdited     =. now
                ]


-- | Display the new post form inself. If the first argument is Just,
-- then use that to prepopulate the form
postForm :: Maybe Post -> Html -> Form Dtek Dtek (FormResult PostEditForm, Widget)
postForm mpost = renderTable $ PostEditForm
    <$> areq uSlugField    fsSlug     (fmap postSlug   mpost)
    <*> areq textField     "Titel"    (fmap postTitle  mpost)
    <*> areq markdownField fsTeaser   (fmap postTeaser mpost)
    <*> areq markdownField "Brödtext" (fmap postBody   mpost)
    <*> areq boolField     fsSumem    (fmap postSumem  mpost)
  where
    fsSlug   = "Slugen"     {fsTooltip = Just "Om titeln är  \"Hacke hackspett\" bör slugen va \"hacke-hackspett\". Slugen är en unik nyckel"}
    fsTeaser = "Teaser"     {fsTooltip = Just "Sammanfattningen som visas på t.ex. förstasidan"}
    fsSumem  = "Konkatenera"{fsTooltip = Just "Sätter innehållet till teaser+brödtext. Annars är innehållet bara brödtexten"}
    slugField = checkBool isEscaped ("Endast bokstäver, siffror o lite till" :: Text) textField
    uSlugField = checkM slugInDb slugField
    isEscaped = all ok_host . T.unpack
    slugInDb t = if Just t == fmap postSlug mpost then return $ Right t else do
        n <- runDB $ count [PostSlug ==. t]
        return $ if n == 0
            then Right t
            else Left ("Det inlägget finns redan" :: Text)

-- These two really don't belong here
postContent :: Post -> Markdown
postContent post = mconcat $ [postTeaser post | postSumem post] ++ [postBody post]

postIsEdited :: Post -> Bool
postIsEdited post = postCreated post /= postEdited post
