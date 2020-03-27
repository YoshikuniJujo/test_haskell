{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.TryReact where

import Control.Monad.State
import Data.Time.Clock.System

import Boxes
import Boxes.Events
import Boxes.Handlers
import MonadicFrp.React
import Field

tryLeftClick :: IO ()
tryLeftClick = do
	f <- openField "tryLeftClick" [exposureMask, buttonPressMask]
	interpret (handleWithoutTime f) (adjust leftClick :: ReactG ()) >>= print
	closeField f

trySameClick :: IO ()
trySameClick = do
	f <- openField "trySameClick" [exposureMask, buttonPressMask]
	interpret (handleWithoutTime f) sameClick >>= print
	closeField f

trySleep :: IO ()
trySleep = do
	f <- openField "trySleep" [exposureMask, buttonPressMask]
	now <- systemToTAITime <$> getSystemTime
	interpret (handle 0.5 f) (adjust $ sleep 3) `runStateT` now >>= print
	closeField f

tryDoubler :: IO ()
tryDoubler = do
	f <- openField "trySleep" [exposureMask, buttonPressMask]
	now <- systemToTAITime <$> getSystemTime
	interpret (handle 0.05 f) doubler `runStateT` now >>= print
	closeField f
