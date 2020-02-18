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
	f <- openField "時間の経過" [exposureMask, buttonPressMask]
	t <- getCurrentTime
	interpretSig (handleDelta 0.05 f) (liftIO . drawRect f) (wiggleRect $ Rect (150, 130) (450, 330))
		`runStateT` t >>= print
	closeField f
