module Main where

import Prelude hiding (break)

import Control.Moffy (break)
import Control.Moffy.Event.Delete (deleteEvent)

import Trial.Boxes (boxes)
import Trial.Boxes.Run (runBoxes)

main :: IO ()
main = () <$ runBoxes "TRY BOXES" (boxes `break` deleteEvent)
