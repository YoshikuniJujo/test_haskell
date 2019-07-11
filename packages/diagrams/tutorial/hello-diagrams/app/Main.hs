module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example :: Diagram B
example = square 1 `atop` circle 1 ||| circle 1 === circle 1

main :: IO ()
main = mainWith example
