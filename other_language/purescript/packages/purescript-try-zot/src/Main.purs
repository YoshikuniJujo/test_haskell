module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import SKI
import Zot

import Data.Maybe
import LambdaToSki
import SkiToZot
import Partial.Unsafe

main :: Effect Unit
main = do
  log "🍝"

interpretLambda :: String -> String -> Effect Unit
interpretLambda lmbd arg = maybe (pure unit) (interpret log)
        <<< ((_ <> arg) <$> _) <<< skiToZot
        <<< show <<< lambdaToSki $ unsafePartial readLambda' lmbd
