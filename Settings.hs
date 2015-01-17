-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings
    ( widgetFile
    , PersistConfig
    , staticRoot
    , staticDir
    , Extra (..)
    , parseExtra
    , development
    , swedishTimeLocale
    , swedishHumanTimeLocale
    ) where

import Control.Applicative
import Data.Default (def)
import Data.Text (Text)
import Data.Time.Format.Human
import Data.Yaml
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax
import Prelude
import System.Locale
import Text.Hamlet
import Text.Shakespeare.Text (st)
import Yesod.Default.Config
import Yesod.Default.Util

import           Paths_DtekPortalen
import Settings.Development


-- | Which Persistent backend this site is using.
type PersistConfig = PostgresConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
getStaticDir :: IO FilePath
getStaticDir = do dir <- getDataFileName staticDir
                  return (if development
                             then "static/"
                             else dir)

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra
    { extraCalendar :: Text -- ^ Google Calendar
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "calendar"
    <*> o .:? "analytics"


swedishTimeLocale :: TimeLocale
swedishTimeLocale = TimeLocale {
  wDays = [ ("söndag", "sön")
          , ("måndag", "mån")
          , ("tisdag", "tis")
          , ("onsdag", "ons")
          , ("torsdag", "tor")
          , ("fredag", "fre")
          , ("lördag", "lör")],
  months = [ ("januari", "jan")
           , ("februari", "feb")
           , ("mars", "mar")
           , ("april", "apr")
           , ("maj", "maj")
           , ("juni", "jun")
           , ("juli", "jul")
           , ("augusti", "aug")
           , ("september", "sep")
           , ("oktober", "okt")
           , ("november", "nov")
           , ("december", "dec")],
  intervals = [ ("år", "år")
              , ("månad", "månader")
              , ("dag", "dagar")
              , ("timme", "timmar")
              , ("minut", "minuter")
              , ("sekund", "sekunder")
              , ("µsekund", "µsekunder")],
  amPm = ("fm", "em"),
  dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
  dateFmt = "%Y-%m-%d",
  timeFmt = "%H:%M:%S",
  time12Fmt = "%I:%M:%S %p"
}

swedishHumanTimeLocale :: HumanTimeLocale
swedishHumanTimeLocale = HumanTimeLocale
    { justNow = "just nu"
    , secondsAgo = \x -> "för " ++ x ++ " sekunder sedan"
    , oneMinuteAgo = "en minut sedan"
    , minutesAgo = \x -> "för " ++ x ++ " minuter sedan"
    , oneHourAgo = "en timme sedan"
    , aboutHoursAgo = \x -> "för cirka " ++ x ++ " timmar sedan"
    , at = \_ x -> "i " ++ x ++ "s"
    , daysAgo = \x -> "för " ++ x ++ " dagar sedan"
    , weekAgo = \x -> "för " ++ x ++ " vecka sedan"
    , weeksAgo = \x -> "för " ++ x ++ " veckor sedan"
    , onYear = id
    , locale = swedishTimeLocale
    , dayOfWeekFmt = "%A"
    , thisYearFmt = "%F"
    , prevYearFmt = "%F"
    }
