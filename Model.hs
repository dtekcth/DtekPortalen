{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Model where

import Prelude
import Yesod
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Either (rights)
import qualified Data.Text as T
import Yesod.Goodies.Markdown
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.List
import Control.Monad (guard)
import Control.Applicative (liftA2)
import Data.Monoid (mconcat, mappend)


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

userCalcName :: User -> Text
userCalcName u = fromMaybe (userIdent u) (userNick u)

-- Forening
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

foreningToListname :: Forening -> String
foreningToListname = show -- This should work, last time I checked

wikiUrl :: Forening -> String
wikiUrl x = "http://dtek.se/wiki/main/" `mappend` show x

instance SinglePiece Forening where
    toSinglePiece x = T.pack $ show x
    fromSinglePiece s =
        case reads $ T.unpack s of
            (x, _):_ -> Just x
            []       -> Nothing

-- Mailman

-- > listsToCids "kalle@anka.se\nrarash@student.chalmers.se\nolle@gmail.com"
-- ["rarash"]
listsToCids :: String -> [String]
listsToCids = map (takeWhile (/='@'))
            . filter (isSuffixOf "@student.chalmers.se")
            . lines

-- Make sure you have `list_members` command available, and that you don't
-- get any permission denied errors when using it with any user.
getMembers :: Forening -> IO(Either String [String])
getMembers forening = do
  (exitCode, sout, serr) <- readProcessWithExitCode "list_members" [maillist] ""
  case exitCode of
    ExitSuccess -> return $ Right $ listsToCids sout
    _           -> return $ Left $ show exitCode ++ ":\n\n" ++ serr
  where
    maillist = foreningToListname forening

-- returns [] on failure
getMemberships :: User -> IO [Forening]
getMemberships u = fmap (map fst
                      . filter snd
                      . rights) $
    mapM (\f -> fmap (fmap ((,) f)) $ g f)  allaForeningar
  where
    g = flip checkMembership u

checkMembership :: Forening -> User -> IO (Either String Bool)
checkMembership f u =
#ifdef PRODUCTION
  fmap (fmap $ elem $ T.unpack $ userIdent u) $ getMembers f
#else
  return $ Right True
#endif

checkMemberships :: [Forening] -> User -> IO (Either String Bool)
checkMemberships fs u = fmap (fmap or . sequence) $ mapM (`checkMembership` u) fs
