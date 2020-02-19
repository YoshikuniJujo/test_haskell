{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import ColoredBoxes
import Handlers

import Signal
import Field

import FieldAndMonadicFrp

main :: IO ()
main = do
	f <- openField "長方形をつくる" [
		exposureMask, buttonPressMask, buttonReleaseMask, button1MotionMask ]
	interpretSig (handleMotion f) (flip (drawRect f) Red) defineRect >>= print
	closeField f
