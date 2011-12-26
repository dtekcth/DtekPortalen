import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withDtek)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withDtek
