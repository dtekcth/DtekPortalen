{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Shorten
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Data.Shorten (Shorten(..)) where

import Data.Text (Text)
import qualified Data.Text as T

class Shorten a where
    shorten :: Int -> a -> a

instance Shorten String where
    shorten n s = if length s > n then take (n - 3) s ++ "..." else s

instance Shorten Text where
    shorten n t = if T.length t > n then T.take (n - 3) t `T.append` "..." else t
