{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Forening where

import Yesod.Dispatch(SinglePiece(..))
import Data.Text (Text)
import qualified Data.Text as T

data Forening = Styret
              | Presidiet
              | DAG
              | SND
              | Delta
              | D6
              | DNollK
              | DRUST
              | DFoto
              | IDrott -- iDrott really
              | Webredax
              | LuDer
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
