{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.TrySig where

import Control.Monad.State
import Data.Time.Clock.System

import TryMyInterface.Boxes
import TryMyInterface.Boxes.Events
import TryMyInterface.Boxes.Handlers
import MonadicFrp.MyInterface
import Field

withInterpretSig :: Show r => String -> (a -> IO ()) -> SigG a r -> IO ()
withInterpretSig fn op s = do
	f <- openField fn [
		exposureMask, buttonPressMask,
		buttonReleaseMask, pointerMotionMask ]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . op) s `runStateT` now >>= print
	closeField f

tryCycleColor :: IO ()
tryCycleColor = withInterpretSig "tryCycleColor" print cycleColor
