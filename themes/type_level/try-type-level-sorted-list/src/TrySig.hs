{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySig where

import Control.Monad.State
import Data.Time.Clock.System

import Sig
import Handlers
import Field

tryCycleColor :: IO ()
tryCycleColor = do
	f <- openField "tryCycleColor" [exposureMask, buttonPressMask, buttonReleaseMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . print) cycleColor `runStateT` now >>= print
	closeField f

tryMousePos :: IO ()
tryMousePos = do
	f <- openField "tryMousePos" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . print) mousePos `runStateT` now >>= print
	closeField f
