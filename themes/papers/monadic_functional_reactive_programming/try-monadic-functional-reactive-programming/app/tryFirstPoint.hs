{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import ColoredBoxes
import Handlers

import React
import Field

main :: IO ()
main = do
	f <- openField "マウスの動き" [
--		exposureMask, buttonPressMask, button1MotionMask ]
		exposureMask, buttonPressMask, pointerMotionMask ]
	interpret (handleMotion f) firstPoint >>= print
	closeField f
