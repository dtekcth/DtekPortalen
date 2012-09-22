-- | Rss Feed Module
-- Implementation inspiration from pbrisbins site. Thanks Patrick!
module Handler.Feed
    ( getFeedR
    ) where

import Import
import Yesod.RssFeed
import Text.Markdown (Markdown)
import Text.Blaze (ToMarkup (toMarkup))

getFeedR :: Handler RepRss
getFeedR = do
    kposts <- runDB $ selectList [] [LimitTo 5, Desc PostCreated]
    feedFromPosts $ map entityVal kposts

feedFromPosts :: [Post] -> Handler RepRss
feedFromPosts posts = do
    rssFeed Feed
        { feedTitle = "Dteks nyhetsfeed"
        , feedAuthor = "Dteks sektioner"
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
        , feedEntryContent = toMarkup $ postContent post
        }
