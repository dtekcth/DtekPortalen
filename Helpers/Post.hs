{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Helpers.Post where

import Import
import Data.Time (getCurrentTime)
import Network.URL
import Text.Markdown
import Data.Time.Format.Human
import qualified Data.Text as T
import Yesod.Text.Markdown (markdownField)
import Settings

slugToPostWidget :: Bool -- ^ Full?
                 -> Text -- ^ The slug
                 -> Widget
slugToPostWidget isFull slug = do
    mpost <- handlerToWidget $ runDB $ selectFirst [PostSlug ==. slug] []
    case mpost of
        Just (entityVal -> post) -> do postToWidget isFull post
        Nothing   -> toWidget [shamlet|Posten med slug "#{slug}" hittades inte!|]

postToWidget :: Bool -- ^ Full?
             -> Post
             -> Widget
postToWidget isFull post = do
    creator <- safeGet postCreator
    editor <-  safeGet postEditor
    prettyCreated <- liftIO $ humanReadableTimeI18N swedishHumanTimeLocale $ postCreated post
    prettyEdited  <- liftIO $ humanReadableTimeI18N swedishHumanTimeLocale $ postEdited post
    if isFull then $(widgetFile "fullpost") else $(widgetFile "teasepost")
  where
    safeGet attr = fmap safeExtract $ handlerToWidget $ runDB $ get (attr post)
    safeExtract = fromMaybe "(borttagen)" . fmap userCalcName

data PostEditForm = PostEditForm
    { formSlug   :: Text
    , formTitle  :: Text
    , formTeaser :: Markdown
    , formBody   :: Markdown
    , formSumem  :: Bool
    }

runPostForm :: Maybe (Entity Post) -> UserId -> Widget
runPostForm mkpost uid = do
    ((res, form), enctype) <-
        handlerToWidget .
        runFormPost .
        postForm $
        fmap entityVal mkpost
    isPreview <- handlerToWidget $ runInputPost $ iopt boolField "preview"
    case res of
        FormSuccess pf ->
            if isPreview /= Just True
                then handlerToWidget $ processFormResult pf
                else do post <- handlerToWidget $ postFromForm pf
                        $(widgetFile "preview")
        _ -> return ()

    [whamlet|
        <div .post_input>
            <form enctype="#{enctype}" method="post">
                <table>
                    ^{form}
                    <tr>
                        <td>&nbsp;
                        <td .buttons>
                            <button type="submit" name="preview" value="yes">Förhandsgranska
                            $maybe _ <- mkpost
                                <input type="submit" value="Uppdatera">
                            $nothing
                                <input type="submit" value="Skapa">

        |]

    where
        processFormResult :: PostEditForm -> Handler ()
        processFormResult pf = do
            p <- postFromForm pf
            case mkpost of
                Just (entityKey -> k) -> do  -- We are going to replace
                    updatePost k p
                    setSuccessMessage "Inlägg uppdaterat!"
                _           -> do --  Post should be inserted
                    _ <- runDB $ insertBy p
                    setSuccessMessage "Inlägg skapat!"
            redirect ManagePostsR

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
postForm :: Maybe Post -> Form PostEditForm
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
