{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Moffy
import Control.Moffy.Event.Mouse.DefaultWindow

import Trial.TryGdk
import Trial.Boxes

main :: IO ()
main = tryGdk @_ @() (const print) $ adjustSig mousePos
