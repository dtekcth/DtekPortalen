module Helpers.Scraping (
     openUrl
   ) where

import Network.HTTP.Enumerator (simpleHttp)
import Data.ByteString.Lazy.Char8 (unpack)

openUrl :: String -> IO String
openUrl = fmap unpack . simpleHttp
