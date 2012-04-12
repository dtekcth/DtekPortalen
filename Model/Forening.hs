module Model.Forening where

import Prelude
import Yesod (PathPiece(..))
import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.List
import Data.Monoid (mappend)
import Model.Persist

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

allForenings :: [Forening]
allForenings = [minBound..maxBound]

foreningToListname :: Forening -> String
foreningToListname = show -- This should work, last time I checked

wikiUrl :: Forening -> String
wikiUrl x = "http://dtek.se/wiki/main/" `mappend` show x

instance PathPiece Forening where
    toPathPiece x = T.pack $ show x
    fromPathPiece s =
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
    mapM (\f -> fmap (fmap ((,) f)) $ g f)  allForenings
  where
    g = flip checkMembership u

checkMembership :: Forening -> User -> IO (Either String Bool)
checkMembership f u =
#ifdef DEVELOPMENT
  return $ Right True
#else
  fmap (fmap $ elem $ T.unpack $ userIdent u) $ getMembers f
#endif

checkMemberships :: [Forening] -> User -> IO (Either String Bool)
checkMemberships fs u = fmap (fmap or . sequence) $ mapM (`checkMembership` u) fs
