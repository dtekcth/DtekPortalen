{-# LANGUAGE ViewPatterns #-}
module CalendarFeed (
   EventInfo(..)
 , getEventInfo
 , CalendarScrapResult
 )
 where

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Time.RFC3339
import Data.Time.LocalTime
import Data.Time.Calendar (addDays)
import Network.URL
import Network.HTTP
import Text.HTML.TagSoup

type CalendarScrapResult = [EventInfo]

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

getUrl :: String -> IO String
getUrl url = do
    start <- fmap showRFC3339 getZonedTime
    end   <- fmap (showRFC3339 . plusOneWeek) getZonedTime
    let completeUrl = (`add_param` ("start-min", start))
                    . (`add_param` ("start-max", end))
                    $ paramUrl
    return $ exportURL completeUrl
  where
    Just basicUrl = importURL url
    paramUrl = basicUrl {
        url_params = [("max-results", "5"), ("orderby", "starttime")
                    , ("sortorder", "ascending"), ("ctz", "Europe/Stockholm")] }

getBody :: String -> IO String
getBody url = fmap UTF8.decodeString $ getUrl url >>= openURL

getEventInfo :: String -- ^ The URL
             -> IO CalendarScrapResult
getEventInfo url = fmap (extractInfo . parseTags) $ getBody url

plusOneWeek :: ZonedTime -> ZonedTime
plusOneWeek (ZonedTime  lt tz) = ZonedTime lt' tz
  where lt' = lt { localDay = addDays 7 (localDay lt) }

finds :: String -> [Tag String] -> [[Tag String]]
finds x tags = map (takeWhile (not . isTagCloseName x)) $
                 partitions (isTagOpenName x) tags

find :: String -> [Tag String] -> [Tag String]
find x = head . finds x

get :: String -> [Tag String] -> String
get s = innerText . find s

data EventInfo = EventInfo {
    title :: String
  , startTime :: ZonedTime
  , endTime :: ZonedTime
  , link :: String
  }
  deriving Show

extractInfo :: [Tag String] -> [EventInfo]
extractInfo tags = [EventInfo t sTime eTime url |
      entry <- finds "entry" tags
    , let t = get "title" entry
    , TagOpen "link" (("rel", "alternate")
                    : ("type", "text/html") : ("href", url) : _) <- entry
    , TagOpen "gd:when" [("endTime", readRFC3339 -> Just eTime)
                       , ("startTime", readRFC3339 -> Just sTime)] <- entry
    ]

