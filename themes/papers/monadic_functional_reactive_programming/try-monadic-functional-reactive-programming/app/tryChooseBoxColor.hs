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
	f <- openField "色を決める" [exposureMask, buttonPressMask]
	t <- getCurrentTime
	interpretSig (handleDelta 0.05 f) (liftIO . drawBox f)
			(chooseBoxColor $ Rect (150, 100) (450, 400))
		`runStateT` t >>= print
	closeField f
