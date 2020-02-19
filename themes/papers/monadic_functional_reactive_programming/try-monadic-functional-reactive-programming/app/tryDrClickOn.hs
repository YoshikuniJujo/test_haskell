{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Time

import ColoredBoxes
import Handlers

import React
import Field

main :: IO ()
main = do
	f <- openField "範囲内でダブルクリックがあったら終了" [
--		exposureMask, buttonPressMask, button1MotionMask ]
		exposureMask, buttonPressMask, pointerMotionMask ]
	t <- getCurrentTime
	interpret (handleDelta 0.05 f) (drClickOn $ Rect (0, 0) (100, 100)) `runStateT` t >>= print
	closeField f
