module Alert where

import Prelude
import Effect

foreign import alert :: String -> Effect Unit
