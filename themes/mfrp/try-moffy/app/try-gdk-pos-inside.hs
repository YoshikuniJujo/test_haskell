{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (repeat)

import Control.Moffy
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Viewable.Shape

import Trial.TryGdk
import Trial.Paper

main :: IO ()
main = tryGdk @_ @() (const print) . adjustSig . repeat $ posInside (Rect (300, 200) (600, 400)) mousePos
