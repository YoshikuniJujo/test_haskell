module Alert where

import Prelude
import Effect

foreign import j_alert :: String -> Effect Unit

alert :: forall a . Show a => a -> Effect Unit
alert = j_alert <<< show
