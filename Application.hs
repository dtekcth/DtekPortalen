{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns #-}
module Application
    ( makeApplication
    , getApplicationDev
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Core.Types as YCT
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.RequestLogger as WL
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit (newManager)
import Control.Monad.Logger (runLoggingT)
import System.Log.FastLogger
import Data.Default.Class

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Forening
import Handler.Posts
import Handler.Profile
import Handler.Admin
import Handler.Document
import Handler.Feed
import Handler.SmallHandlers

-- Own imports
import Helpers.Scraping (hourlyRefreshingRef)
import Scrapers.CalendarFeed
import Data.Text (unpack)


-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = WL.Logger . loggerSet $ appLogger foundation
        }

    app <- toWaiAppPlain foundation
    return $ logWare app

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager defaultManagerSettings
    s <- staticSite
    let calendarUrl = unpack . extraCalendar $ appExtra conf
    calendarRef <- hourlyRefreshingRef (getEventInfo calendarUrl) []
    let cachedValues = CachedValues calendarRef

    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConfig)

    ourLoggerSet <- newStdoutLoggerSet defaultBufSize
    (dateCacheGetter, _) <- clockDateCacher
    let logger = YCT.Logger ourLoggerSet dateCacheGetter

    let foundation = App conf s p manager dbconf logger cachedValues
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)
    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
