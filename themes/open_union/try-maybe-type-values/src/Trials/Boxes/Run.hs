{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.Run (runBoxes) where

import Control.Monad.State (runStateT, liftIO)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)

import MonadicFrp.Run (interpretSt)
import Field (
	Field, openField, closeField,
	exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask )
import Trials.Boxes.Event
import Trials.Boxes.Handle (handleBoxes, Mode(..))
import Trials.Boxes.View

runBoxes :: Show r => String -> SigG [Box] r -> IO ()
runBoxes ttl = withInterpretSig ttl drawBoxes

withInterpretSig ::
	Show r => String -> (Field -> a -> IO ()) -> SigG a r -> IO ()
withInterpretSig fn op s = do
	f <- openField fn [
		exposureMask, buttonPressMask,
		buttonReleaseMask, pointerMotionMask ]
	now <- systemToTAITime <$> getSystemTime
	print =<< interpretSt InitMode (handleBoxes 0.05 f) (liftIO . op f) s `runStateT` now
	closeField f
