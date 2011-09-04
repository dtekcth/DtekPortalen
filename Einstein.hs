{-# LANGUAGE OverloadedStrings #-}
module Einstein (scrapEinstein, EinsteinScrapResult) where

import Network.HTTP
import Text.HTML.TagSoup
import Data.List.Split (splitEvery)
import Control.Monad (guard, liftM2)
import Data.Char (isNumber)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T

-- I use tag soup as it apperently fixes unicode characters
type EinsteinScrapResult = Maybe (Int, [[String]])

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

scrapEinstein :: IO EinsteinScrapResult
scrapEinstein = do
    body <- openURL "http://www.butlercatering.se/einstein.html"
    return (parse body)
                   
printMenuToStdout :: IO()
printMenuToStdout = do
    Just (week, sss) <- scrapEinstein 
    putStrLn $ "\nVecka " ++ show week ++ ":"
    mapM_ (putStrLn . unlines) sss

parse :: String -> Maybe (Int, [[String]])
parse body = liftM2 (,) mweek mmenu
  where
    tags = parseTags body
    goodContents = filter (elem '•') [ s | TagText s <- tags ]
    cleanContents = map (dropWhile (`elem` " •\n\r")) goodContents
    splitted = splitEvery 3  cleanContents
    mmenu = guard (map length splitted == [3, 3, 3, 3, 3]) >> Just splitted
    textNum = T.takeWhile isNumber $ snd $ T.breakOnEnd "Meny V " (T.pack body)
    mweek = fmap fst $ listToMaybe $ reads $ T.unpack textNum
