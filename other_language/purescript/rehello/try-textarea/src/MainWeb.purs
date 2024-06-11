module MainWeb where

import Prelude
import Control.Monad
import Data.Unit
import Effect
import Effect.Console

import Test

main :: Effect Unit
main = do
        log "foo"
        log =<< js_getElementTypeStrById "lambda-area"
        log =<< js_getElementTypeStrById "lambda-to-ski"
        log =<< js_getElementTypeStrById "ski-area"
