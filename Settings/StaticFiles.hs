module Settings.StaticFiles where

import           Control.Monad (return)
import           Data.Default (def)
import           Language.Haskell.TH (Q, Exp, Name, runIO)
import           Prelude (IO, FilePath)
import           Yesod.Static
import qualified Yesod.Static as Static

import           Settings (getStaticDir)
import           Settings.Development


-- | use this to create your static file serving site
staticSite :: IO Static.Static
staticSite =
  do dir <- getStaticDir
     (if development
         then Static.staticDevel
         else Static.static) dir

-- | This generates easy references to files in the static directory at compile time,
-- giving you compile-time verification that referenced files exist.
-- Warning: any files added to your static directory during run-time can't be
-- accessed this way. You'll have to use their FilePath or URL to access them.
$(runIO getStaticDir >>= staticFiles)

combineSettings :: CombineSettings
combineSettings = def

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets' development combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts' development combineSettings
