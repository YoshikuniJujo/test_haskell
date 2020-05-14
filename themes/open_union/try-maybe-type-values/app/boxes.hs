{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Trials.Boxes
import Trials.Boxes.Run

main :: IO ()
main = runBoxes "Boxes" boxes
