{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import ColoredBoxes
import Handlers

import Signal
import Field

main :: IO ()
main = do
	f <- openField "マウスの動き" [
		exposureMask, buttonPressMask, button1MotionMask ]
	interpretSig (handleMotion f) print mousePos
	closeField f
