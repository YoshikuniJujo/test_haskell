{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import ColoredBoxes
import Handlers

import Signal
import Field

main :: IO ()
main = do
	f <- openField "長方形をつくる" [
		exposureMask, buttonPressMask, button1MotionMask ]
	interpretSig (handleMotion f) (drawRect f) (curRect (100, 80))
	closeField f

drawRect :: Field -> Rect -> IO ()
drawRect f rct = do
	clearField f
	fillRect f 0xff0000 l u w h
	flushField f
	where
	(l_, u_, w_, h_) = leftupAndSize rct
	[l, u] = fromIntegral <$> [l_, u_]
	[w, h] = fromIntegral <$> [w_, h_]
