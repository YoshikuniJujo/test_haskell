module Test where

import Effect

foreign import js_getElementTypeStrById :: String -> Effect String
