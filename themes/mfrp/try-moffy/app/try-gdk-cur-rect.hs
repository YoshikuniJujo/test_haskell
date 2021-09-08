{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Moffy

import Trial.TryGdk
import Trial.Paper
import Trial.Paper.Gdk3

main :: IO ()
main = tryGdk @_ @() showRect . adjustSig $ curRect (100, 100)
