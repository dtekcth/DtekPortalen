-- | A module with handlers that don't deserve their own modules
module Handler.SmallHandlers where

import Import
import Text.Markdown

layout :: Html -> Widget -> Handler RepHtml
layout title widget = standardLayout $ do
    setDtekTitle title
    addWidget widget

getSNDR :: Handler RepHtml
getSNDR = do
    md <- documentFromDB "stat_sndfullpage"
    layout "SNDs snabbinfosida" $(widgetFile "snd")

getAboutR :: Handler RepHtml
getAboutR = layout "Om Portalen" $(widgetFile "about")

getLinksR :: Handler RepHtml
getLinksR = layout "Länksamling" $(widgetFile "links")

getContactR :: Handler RepHtml
getContactR = layout "Kontakt" $(widgetFile "contact")
