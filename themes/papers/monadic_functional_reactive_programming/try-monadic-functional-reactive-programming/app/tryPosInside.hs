{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import ColoredBoxes
import Handlers

import React
import Field

main :: IO ()
main = do
	f <- openField "ドラッグすると終了する" [
		exposureMask, buttonPressMask, button1MotionMask ]
	interpret (handleMotion f) (posInside (Rect (0, 0) (100, 100)) mousePos) >>= print
	closeField f
