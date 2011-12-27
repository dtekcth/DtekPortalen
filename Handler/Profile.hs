{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Profile where

import Import
import Data.Maybe (fromMaybe)

type UserFormResult = Maybe Text

profileEditForm :: User -> Form UserFormResult
profileEditForm u = renderTable $ aopt textField "Användarnamn"
        { fsTooltip = Just "Visas t.ex. vid nyhetsinlägg"
        } (Just $ userNick u)


getProfileR :: Handler RepHtml
getProfileR = do
    (_, u) <- requireAuth
    let usernick = fromMaybe "" $ userNick u
    ((_, form), enctype) <- runFormPost $ profileEditForm u
    defaultLayout $ do
        [whamlet|
            <h1>Redigera
            <article .fullpage .profile
                <form enctype="#{enctype}" method="post">
                    <table>
                        ^{form}
                        <tr>
                            <td>&nbsp;
                            <td .buttons>
                                <input type="submit" value="Spara">
            |]
        setDtekTitle "Redigera profil"


postProfileR :: Handler RepHtml
postProfileR = do
    (uid, u) <- requireAuth
    ((res, _ ), _ ) <- runFormPost $ profileEditForm u
    case res of
        FormSuccess ef -> saveChanges uid ef
        _ -> return ()
    getProfileR

    where
        saveChanges :: UserId -> UserFormResult -> Handler ()
        saveChanges uid ef = do
            runDB $ update uid
                [ UserNick =. ef
                ]

            setSuccessMessage "Profil sparad"
            tm <- getRouteToMaster
            redirect RedirectTemporary $ tm ProfileR

