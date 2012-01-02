module Handler.SmallHandlers where

import Import
import Yesod.Markdown

layout :: Html -> Widget -> Handler RepHtml
layout title widget = standardLayout $ do
    setDtekTitle title
    addWidget widget

getSNDR :: Handler RepHtml
getSNDR = do
    md <- documentFromDB "stat_sndfullpage"
    layout "SNDs snabbinfosida" $(widgetFile "snd")
