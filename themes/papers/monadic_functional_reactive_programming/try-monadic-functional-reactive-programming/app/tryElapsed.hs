{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Time

import ColoredBoxes
import Signal
import Field

import Handlers

main :: IO ()
main = do
	f <- openField "時間の経過" [exposureMask, buttonPressMask]
	t <- getCurrentTime
	interpretSig (handle f) (liftIO . print) elapsed `runStateT` t >>= print
	closeField f
