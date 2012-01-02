module Model.Document where

import Prelude
import Yesod (SinglePiece(..))
import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as T
import Yesod.Markdown
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.List
import Data.Monoid (mappend)
import Model.Persist


emptyDoc :: Text -> Document
emptyDoc tid = Document tid ""

specialDocTids :: [Text]
specialDocTids = []

validDocTid :: Text -> Bool
validDocTid "sandbox" = True
validDocTid tid = tid `elem` specialDocTids
