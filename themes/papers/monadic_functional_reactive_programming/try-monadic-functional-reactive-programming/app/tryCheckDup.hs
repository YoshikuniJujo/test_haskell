{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Time

import React
import Freer
import Field

import ColoredBoxes
import Handlers

main :: IO ()
main = do
	f <- openField "右ダブルクリックのあとに左クリックが2回あったら終了" [
		exposureMask, buttonPressMask, pointerMotionMask ]
	t <- getCurrentTime
	(print =<<) . (`runStateT` t) $ runCount do
		cd <- addTag checkDup
		pure $ interpret (handleDelta 0.05 f) do
			checkDup `first` checkDup >>= \case
				(Pure _, Pure _) -> pure ()
				_ -> pure ()
			cd `first` cd >>= \case
				(Pure _, Pure _) -> pure ()
				_ -> pure ()
	closeField f
