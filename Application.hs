{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withDtek
    , withDevelAppPort
    ) where

import Foundation
import Settings
import Settings.StaticFiles (static)
import Yesod.Auth
import Yesod.Logger (makeLogger, flushLogger, Logger, logString, logLazyText)
import Database.Persist.GenericSql
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, toDyn)
import Network.Wai.Middleware.Debug (debugHandle)

#ifndef WINDOWS
import qualified System.Posix.Signals as Signal
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
#endif

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Forening
import Handler.Links
import Handler.Posts
import Handler.Profile

-- Egna imports
import Data.IORef
import Einstein
import CalendarFeed
import Control.Monad (forever)
import Control.Concurrent (threadDelay)


-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Dtek" resourcesDtek

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withDtek :: AppConfig -> Logger -> (Application -> IO a) -> IO ()
withDtek conf logger f = do
    s <- static Settings.staticDir
    einsteinRef <- hourlyRefreshingRef scrapEinstein
    let calendarUrl = "https://www.google.com/calendar/feeds/pbtqihgenalb8s3eddsgeuo1fg%40group.calendar.google.com/public/full"
    calendarRef <- hourlyRefreshingRef (getEventInfo calendarUrl)
    let cachedValues = CachedValues einsteinRef calendarRef
    Settings.withConnectionPool conf $ \p -> do
        runConnectionPool (runMigration migrateAll) p
        let h = Dtek conf logger s p cachedValues
#ifdef WINDOWS
        toWaiApp h >>= f >> return ()
#else
        tid <- forkIO $ toWaiApp h >>= f >> return ()
        flag <- newEmptyMVar
        _ <- Signal.installHandler Signal.sigINT (Signal.CatchOnce $ do
            putStrLn "Caught an interrupt"
            killThread tid
            putMVar flag ()) Nothing
        takeMVar flag
#endif

-- For einsteinscraping and such
hourlyRefreshingRef :: IO a -> IO (IORef a)
hourlyRefreshingRef io = do
    ref <- io >>= newIORef
    forkIO $ forever $ (writeIORef ref =<< io) >> threadDelay (3600*1000)
    return ref

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort =
    toDyn go
  where
    go :: ((Int, Application) -> IO ()) -> IO ()
    go f = do
        conf <- Settings.loadConfig Settings.Development
        let port = appPort conf
        logger <- makeLogger
        logString logger $ "Devel application launched, listening on port " ++ show port
        withDtek conf logger $ \app -> f (port, debugHandle (logHandle logger) app)
        flushLogger logger
      where
        logHandle logger msg = logLazyText logger msg >> flushLogger logger
