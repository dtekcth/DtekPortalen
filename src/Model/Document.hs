module Model.Document where
-- | Documents are kept in a non-type safe way for simplicity, that is
-- document types (static, user-owned, forening-owned) ones are
-- indistinguishable by value. Instead we use convention of textual id:
--
--  * prefix `stat_` means a statical by compile time known id
--
-- No other types are used yet, but can become so later.

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
  , g "dagads" "Reklam som kan sättas upp av DAG" [DAG]
  ]
  where g x y z = ("stat_" <> x, ("Dokument `" <> x <> "`: " <> y, z))
        (<>)    = T.append

documentPrivileges :: [(Text, [Forening])]
documentPrivileges = map (fmap snd) documentsList

documentDescriptions :: [(Text, Text)]
documentDescriptions = map (fmap fst) documentsList

specialDocTids :: [Text]
specialDocTids = map fst documentsList

validDocTid :: Text -> Bool
validDocTid "stat_sandbox" = True
validDocTid tid = tid `elem` specialDocTids

emptyDoc :: Text -> Document
emptyDoc tid = Document tid ""
