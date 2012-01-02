{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withDtek
    , withDevelAppPort
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger, logBS, flushLogger)
import Network.Wai.Middleware.RequestLogger
import Data.Dynamic (Dynamic, toDyn)
import qualified Database.Persist.Base
import Database.Persist.GenericSql (runMigration)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Forening
import Handler.Posts
import Handler.Profile
import Handler.Admin
import Handler.Document
import Handler.SmallHandlers

-- Own imports
import Helpers.Scraping (hourlyRefreshingRef)
import Scrapers.Einstein
import Scrapers.CalendarFeed
import Config


-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Dtek" resourcesDtek

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withDtek :: AppConfig DefaultEnv Extra -> Logger -> (Application -> IO a) -> IO ()
withDtek conf logger f = do
    s <- staticSite
    einsteinRef <- hourlyRefreshingRef scrapEinstein Nothing
    let calendarUrl = appExtra conf
    calendarRef <- hourlyRefreshingRef (getEventInfo calendarUrl) []
    let cachedValues = CachedValues einsteinRef calendarRef
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
            $ either error return . Database.Persist.Base.loadConfig
    Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        Database.Persist.Base.runPool dbconf (runMigration migrateAll) p
        let h = Dtek conf logger s p cachedValues
        defaultRunner (f . logWare) h
  where
#ifdef DEVELOPMENT
    logWare = logHandleDev (\msg -> logBS logger msg >> flushLogger logger)
#else
    logWare = logStdout
#endif

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelAppWith myDevelopmentConfig withDtek
  where myDevelopmentConfig :: IO (AppConfig DefaultEnv Extra)
        myDevelopmentConfig = fmap (\c -> c { appExtra = extra }) loadDevelopmentConfig
        extra = "https://www.google.com/calendar/feeds/datateknologsektionen@gmail.com/public/full"
