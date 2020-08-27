{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Run (
	-- * runBoxes
	runBoxes ) where

import Control.Moffy.Run (interpretSt)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)

import Trial.Boxes.Handle (SigB, Mode(..), handleBoxes)
import Trial.Boxes.View (Box, drawBoxes)
import Field (
	openField, closeField,
	exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask )

---------------------------------------------------------------------------

runBoxes :: String -> SigB s [Box] r -> IO r
runBoxes ttl s = do
	f <- openField ttl [
		exposureMask,
		buttonPressMask, buttonReleaseMask, pointerMotionMask ]
	(r, _) <- interpretSt (handleBoxes 0.05 f) (drawBoxes f) s
		. (InitialMode ,) . systemToTAITime =<< getSystemTime
	r <$ closeField f
