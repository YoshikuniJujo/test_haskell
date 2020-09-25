module Main where

import Prelude hiding (break)

import Control.Moffy (adjust, waitFor, break)
import Control.Moffy.Event.Delete (deleteEvent)
import Control.Moffy.Event.Window

import Trial.Boxes (boxes)
import Trial.Boxes.RunXField (runBoxes)

main :: IO ()
-- main = () <$ runBoxes "TRY BOXES" (waitFor (adjust windowNew) >> boxes `break` deleteEvent)
main = () <$ runBoxes "TRY BOXES" (boxes `break` deleteEvent)
