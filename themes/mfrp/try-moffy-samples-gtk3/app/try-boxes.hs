module Main where

import Control.Moffy
import Control.Moffy.Samples.Boxes
import Control.Moffy.Samples.Boxes.Run.Gtk3

main :: IO ()
main = runBoxes $ waitFor leftClick
