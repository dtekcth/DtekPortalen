-- | A module with handlers that don't deserve their own modules
module Handler.SmallHandlers where

import Import
import Text.Markdown

layout :: Html -> Widget -> Handler Html
layout title widget = standardLayout $ do
    setDtekTitle title
    widget

getSNDR :: Handler Html
getSNDR = do
    md <- documentFromDB "stat_sndfullpage"
    layout "SNDs snabbinfosida" $(widgetFile "snd")

getAboutR :: Handler Html
getAboutR = layout "Om Portalen" $(widgetFile "about")

getLinksR :: Handler Html
getLinksR = layout "Länksamling" $(widgetFile "links")

getContactR :: Handler Html
getContactR = layout "Kontakt" $(widgetFile "contact")
