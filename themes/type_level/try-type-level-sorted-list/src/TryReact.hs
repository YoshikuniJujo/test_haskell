{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryReact where

import Control.Monad.State
import Data.Time.Clock.System

import Boxes

import React
import Handlers
import Field

tryLeftClick :: IO ()
tryLeftClick = do
	f <- openField "tryLeftClick" [exposureMask, buttonPressMask]
	interpret (handleWithoutTime f) (adjust leftClick) >>= print
	closeField f

trySameClick :: IO ()
trySameClick = do
	f <- openField "trySameClick" [exposureMask, buttonPressMask]
	interpret (handleWithoutTime f) sameClick >>= print
	closeField f

tryLeftUp :: IO ()
tryLeftUp = do
	f <- openField "tryLeftClick" [exposureMask, buttonPressMask, buttonReleaseMask]
	interpret (handleWithoutTime f) (adjust leftUp) >>= print
	closeField f

tryDoubler :: IO ()
tryDoubler = do
	f <- openField "tryDoubler" [exposureMask, buttonPressMask, buttonReleaseMask]
	now <- systemToTAITime <$> getSystemTime
	interpret (handle 0.1 f) doubler `runStateT` now >>= print
	closeField f
