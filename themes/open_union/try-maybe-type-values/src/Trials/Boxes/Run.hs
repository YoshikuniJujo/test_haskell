{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.Run (runBoxes) where

import Control.Monad.State (runStateT, liftIO)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)

import MonadicFrp.Run (interpretSt)
import Trials.Boxes.Event (SigG)
import Trials.Boxes.Handle (Mode(..), handleBoxes)
import Trials.Boxes.View (Box, drawBoxes)
import Field (
	Field, openField, closeField,
	exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask )

---------------------------------------------------------------------------

runBoxes :: Show r => String -> SigG [Box] r -> IO ()
runBoxes ttl = withInterpretSig ttl drawBoxes

withInterpretSig :: Show r =>
	String -> (Field -> a -> IO ()) -> SigG a r -> IO ()
withInterpretSig fn op s = do
	f <- openField fn [
		exposureMask, buttonPressMask,
		buttonReleaseMask, pointerMotionMask ]
	print	=<< (interpretSt InitMode (handleBoxes 0.05 f) (liftIO . op f) s
			`runStateT`) . systemToTAITime =<< getSystemTime
	closeField f
