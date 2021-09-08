{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Moffy
import Control.Moffy.Viewable.Shape

import Trial.TryGdk
import Trial.Boxes

main :: IO ()
main = tryGdk @_ @() showRect . adjustSig . wiggleRect $ Rect (300, 200) (600, 400)
