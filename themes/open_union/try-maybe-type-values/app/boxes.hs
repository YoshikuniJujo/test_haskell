{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Trials.Boxes (boxes)
import Trials.Boxes.Run (runBoxes)

main :: IO ()
main = runBoxes "Boxes" boxes
