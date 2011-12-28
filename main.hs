import Yesod.Default.Main   (defaultMain)
import Application          (withDtek)
import Prelude              (IO)
-- Own imports below
import Yesod.Default.Config (fromArgsExtra)
import Config (loadExtra)

main :: IO ()
main = defaultMain (fromArgsExtra loadExtra) withDtek
