module Main where

import Prelude hiding (break)

import Control.Moffy
import Control.Moffy.Event.Delete

import Trial.Boxes
import Trial.Boxes.Run

main :: IO ()
main = () <$ trySigGBoxes' "TRY BOXES" (boxes `break` deleteEvent)
