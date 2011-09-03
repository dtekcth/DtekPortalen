{-# LANGUAGE ViewPatterns #-}
module CalendarFeed (
   EventInfo(..)
 , getEventInfo
 )
 where

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Time.RFC3339
import Data.Time.LocalTime
import Data.Time.Calendar (addDays)
import Network.URL
import Network.HTTP
import Text.HTML.TagSoup

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

getUrl = do
    startTime <- fmap showRFC3339 getZonedTime
    endTime   <- fmap (showRFC3339 . plusOneWeek) getZonedTime
    let completeUrl = (`add_param` ("start-min", startTime))
                    . (`add_param` ("start-max", endTime))
                    $ paramUrl
    return $ exportURL completeUrl
  where
    Just basicUrl = importURL
     "https://www.google.com/calendar/feeds/pbtqihgenalb8s3eddsgeuo1fg%40group.calendar.google.com/public/full"
    paramUrl = basicUrl {
        url_params = [("max-results", "5"), ("orderby", "starttime")
                    , ("sortorder", "ascending"), ("ctz", "Europe/Stockholm")] }

getUrlBody = fmap UTF8.decodeString $ getUrl >>= openURL
getEventInfo = fmap (extractInfo . parseTags) getUrlBody

plusOneWeek :: ZonedTime -> ZonedTime
plusOneWeek (ZonedTime  lt tz) = ZonedTime lt' tz
  where lt' = lt { localDay = addDays 7 (localDay lt) }


finds x tags = map (takeWhile (not . isTagCloseName x)) $
                 partitions (isTagOpenName x) tags
find x = head . finds x
get s = innerText . find s

data EventInfo = EventInfo {
    title :: String
  , startTime :: ZonedTime
  , endTime :: ZonedTime
  , link :: String
  }
  deriving Show

extractInfo :: [Tag String] -> [EventInfo]
extractInfo tags = [EventInfo title startTime endTime link |
      entry <- finds "entry" tags
    , let title = get "title" entry
    , TagOpen "link" (("rel", "alternate")
                    : ("type", "text/html") : ("href", link) : _) <- entry
    , TagOpen "gd:when" [("endTime", readRFC3339 -> Just endTime)
                       , ("startTime", readRFC3339 -> Just startTime)] <- entry
    ]

