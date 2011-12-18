module Helpers.Scraping (
     openUrl
   , hourlyRefreshingRef
   ) where

import Network.HTTP.Enumerator (simpleHttp)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.IORef
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)

-- | Given url, return site contents
openUrl :: String -> IO String
openUrl = fmap unpack . simpleHttp

-- For einsteinscraping and such
hourlyRefreshingRef :: IO a         -- ^ Routine to run every hour to fill in ref
                    -> a            -- ^ Start value
                    -> IO (IORef a) -- ^ Ref one can read from
hourlyRefreshingRef io a = do
    ref <- newIORef a
    forkIO $ forever $ do forkIO (writeIORef ref =<< io)
                          threadDelay (3600*1000)
    return ref
