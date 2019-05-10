module Main where

import Data.Default
import Graphics.Rendering.Chart.Backend.Diagrams

import Lib

main :: IO ()
main = toFile def "example.svg" chart
