module Model.Document where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Model.Forening
import Model.Persist

-- | [(textual id, (description, [allowed forenings]))]
--
-- List of all compile time known documents, but there can be more
-- like each user or forening has a private document.
documentsList :: [(Text, (Text, [Forening]))]
documentsList =  [
    g "sndfullpage" "SNDs publika sida med snabbinfo med scheman o.s.v." [SND]
  , g "sndfrontpage" "Snabblänkar med scheman o.s.v.. Visas på förstasidan!" [SND]
  ]
  where g x y z = (x, ("Dokument " ++ x ++ ": " ++ y, z))
        (++)    = T.append

documentPrivileges :: [(Text, [Forening])]
documentPrivileges = map (fmap snd) documentsList

documentDescriptions :: [(Text, Text)]
documentDescriptions = map (fmap fst) documentsList

specialDocTids :: [Text]
specialDocTids = map fst documentsList

validDocTid :: Text -> Bool
validDocTid "sandbox" = True
validDocTid tid = tid `elem` specialDocTids

emptyDoc :: Text -> Document
emptyDoc tid = Document tid ""
