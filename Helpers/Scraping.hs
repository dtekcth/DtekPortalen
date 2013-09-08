module Helpers.Scraping (
     openUrl
   , hourlyRefreshingRef
   ) where

import Prelude
import Network.HTTP.Conduit (simpleHttp)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.IORef
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)

-- | Given url, return site contents
openUrl :: String -> IO String
openUrl = fmap unpack . simpleHttp

-- For scraping
hourlyRefreshingRef :: IO a         -- ^ Routine to run every hour to fill in ref
                    -> a            -- ^ Start value
                    -> IO (IORef a) -- ^ Ref one can read from
hourlyRefreshingRef io a = do
    ref <- newIORef a
    _ <- forkIO $ forever $ do _ <- forkIO (writeIORef ref =<< io)
                               let seconds = 60
                               threadDelay (seconds*1000000)
    return ref
