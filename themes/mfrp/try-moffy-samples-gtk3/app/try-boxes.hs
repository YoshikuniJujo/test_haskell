module Main where

import Data.Type.Flip

import Control.Moffy
import Control.Moffy.Samples.Boxes
import Control.Moffy.Samples.Boxes.Viewable
import Control.Moffy.Samples.Boxes.Run.Gtk3

main :: IO ()
main = runBoxes $ (: []) . (`Box` Red) <$%> curRect (100, 100)
