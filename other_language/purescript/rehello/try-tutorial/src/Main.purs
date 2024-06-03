module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)

import Data.Number (sqrt)
import Prelude

diagonal w h = sqrt (w * w + h * h)

main :: Effect Unit
main = logShow (diagonal 3.0 4.0)

add :: Int -> Int -> Int
add x y = x + y

example x y z = foo + bar
        where
        foo = x * y
        bar = y * z
