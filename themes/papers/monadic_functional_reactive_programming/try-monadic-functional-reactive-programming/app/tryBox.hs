{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Time

import ColoredBoxes
import Signal
import Field

import Handlers
import FieldAndMonadicFrp

main :: IO ()
main = do
	f <- openField "ひとつの箱" [
		exposureMask,
		buttonPressMask, buttonReleaseMask, pointerMotionMask ]
	t <- getCurrentTime
	interpretSig (handleDelta 0.05 f) (liftIO . drawBox f) box `runStateT` t
		>>= print
	closeField f
