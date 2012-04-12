{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)

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
import Scrapers.Einstein
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
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication conf logger = do
    foundation <- makeFoundation conf setLogger
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

makeFoundation :: AppConfig DefaultEnv Extra -> Logger -> IO App
makeFoundation conf setLogger = do
    manager <- newManager def
    s <- staticSite
    einsteinRef <- hourlyRefreshingRef scrapEinstein Nothing
    let calendarUrl = unpack . extraCalendar $ appExtra conf
    calendarRef <- hourlyRefreshingRef (getEventInfo calendarUrl) []
    let cachedValues = CachedValues einsteinRef calendarRef

    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    return $ App conf setLogger s p manager dbconf cachedValues

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
