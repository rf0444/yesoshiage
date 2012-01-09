import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withYO)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withYO