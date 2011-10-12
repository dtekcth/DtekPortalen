{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Forening where

import Yesod.Dispatch(SinglePiece(..))
import Data.Text (Text)
import qualified Data.Text as T

data Forening = Delta
              | Drust
              | DNollK
              | DAG
              | SND
              | Webredax
  deriving (Show, Read, Enum, Bounded, Eq )

allaForeningar :: [Forening]
allaForeningar = [minBound..maxBound]

checkMembership :: Forening -> IO Bool
checkMembership f = return True

checkMemberships :: [Forening] -> IO Bool
checkMemberships = fmap or . mapM checkMembership

instance SinglePiece Forening where
    toSinglePiece x = T.pack $ show x
    fromSinglePiece s =
        case reads $ T.unpack s of
            (x, _):_ -> Just x
            []       -> Nothing
