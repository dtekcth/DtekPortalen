{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Document where

import Import
import Yesod.Markdown

type DocumentFormResult = Markdown

documentEditForm :: Document -> Form DocumentFormResult
documentEditForm doc = renderTable $
  areq markdownField "Innehåll" (Just $ documentContent doc)

-- | Short-circuit if the document doesn't exist,
--   if it does exist return the DocumentId and Document
guardExistence :: Text -- ^ Textual ID of the document
               -> Handler (Entity Document) --(DocumentId, Document)
guardExistence tid | not (validDocTid tid) =
    hamletToRepHtml [hamlet| Ogiltigt dokument #{tid} |] >>= sendResponse
guardExistence tid | otherwise = do
    e <- runDB $ insertBy defDoc
    return $ either id (flip Entity defDoc) e
  where defDoc = emptyDoc tid

getDocumentR :: Text -- ^ Textual ID of the document
          -> Handler RepHtml
getDocumentR tid = do
    doc <- fmap entityVal $ guardExistence tid
    ((_, form), enctype) <- runFormPost $ documentEditForm doc
    defaultLayout $ do
        [whamlet|
            <h1>Redigera
            <p> Ingen historik (förutom backups) finns, redigera varsamt!
            <article .fullpage .document>
                <form enctype="#{enctype}" method="post">
                    <table>
                        ^{form}
                        <tr>
                            <td>&nbsp;
                            <td .buttons>
                                <input type="submit" value="Spara">
            |]
        setDtekTitle "Redigera dokument"
        -- $(widgetFile "document") -- TODO: why left this empty?

postDocumentR :: Text -- ^ Textual ID of the document
           -> Handler RepHtml
postDocumentR tid = do
    Entity did doc <- guardExistence tid
    ((res, _), _) <- runFormPost $ documentEditForm doc
    case res of
        FormSuccess formRes -> saveChanges did formRes
        _ -> return ()
    getDocumentR tid

    where
        saveChanges :: DocumentId -> DocumentFormResult -> Handler ()
        saveChanges did ef = do
            runDB $ update did
                [ DocumentContent =. ef
                ]
            setSuccessMessage "Dokument sparad"
