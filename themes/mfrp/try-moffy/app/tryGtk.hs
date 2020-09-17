{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Control.Moffy
import Control.Moffy.Event.Delete
import Trial.Boxes
import Trial.Boxes.RunGtkField

main :: IO ()
main = () <$ runBoxes (boxes `break` deleteEvent)
