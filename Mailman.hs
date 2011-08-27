module Mailman where

import Forening
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.List

-- Before using this module:
-- Make sure you have `list_members` command available, and that you don't
-- get any permission denied errors when using it with any user.

-- > listsToCids "kalle@anka.se\nrarash@student.chalmers.se\nolle@gmail.com"
-- ["rarash"]
listsToCids :: String -> [String]
listsToCids = map (takeWhile (/='@'))
            . filter (isSuffixOf "@student.chalmers.se")
            . lines

getMembers :: Forening -> IO(Either String [String])
getMembers forening = do
  (exitCode, sout, serr) <- readProcessWithExitCode "list_members" [maillist] ""
  case exitCode of
    ExitSuccess -> return $ Right $ listsToCids sout
    _           -> return $ Left $ show exitCode ++ ":\n\n" ++ serr
  where
    maillist = foreningToListname forening

foreningToListname :: Forening -> String
foreningToListname = show -- This should work, last time I checked
