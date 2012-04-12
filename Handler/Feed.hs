-- | Rss Feed Module
-- Implementation inspiration from pbrisbins site. Thanks Patrick!
module Handler.Feed
    ( getFeedR
    ) where

import Import
import Yesod.RssFeed
import Yesod.Markdown (Markdown, markdownToHtml)
import Text.Blaze (preEscapedText)
import Text.Blaze.Internal (HtmlM (Append))

getFeedR :: Handler RepRss
getFeedR = do
    kposts <- runDB $ selectList [] [LimitTo 5, Desc PostCreated]
    feedFromPosts $ map entityVal kposts

feedFromPosts :: [Post] -> Handler RepRss
feedFromPosts posts = do
    rssFeed Feed
        { feedTitle = "Dteks nyhetsfeed"
        , feedDescription = "Nyheter från Chalmers Datateknologsektion"
        , feedLanguage = "sv"
        , feedLinkSelf = FeedR
        , feedLinkHome = RootR
        , feedUpdated = postEdited $ head posts
        , feedEntries = map postToRssEntry posts
        }

postToRssEntry :: Post -> FeedEntry (Route App)
postToRssEntry post = FeedEntry
        { feedEntryLink = PostR $ postSlug post
        , feedEntryUpdated = postEdited post
        , feedEntryTitle = postTitle post
        , feedEntryContent = cdata $ postContent post
        }
  where
    -- Should appear as formatted HTML in readers that support
    -- that.Rss validation errors on script tage used by
    -- markdown conversion to obfuscate an email. Looks pretty
    -- bad in snownews (but readable) -- not sure about Google
    -- yet.
    cdata :: Markdown -> Html
    cdata mkd = let prefix = preEscapedText "<![CDATA["
                    content = markdownToHtml mkd
                    suffix = preEscapedText "]]>" in Append prefix $ Append content suffix
