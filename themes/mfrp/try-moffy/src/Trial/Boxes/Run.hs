{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Run (
	-- * runBoxes
	runBoxes ) where

import Control.Moffy.Handle
import Control.Moffy.Handle.Time
import Control.Moffy.Handle.XField
import Control.Moffy.Run (interpretSt)
import Data.Time
import Data.Time.Clock.TAI
import Data.Time.Clock.System (getSystemTime, systemToTAITime)

import Trial.Boxes.BoxEv
import Trial.Boxes.View (Box, drawBoxes)
import Field (
	Field, openField, closeField,
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

handleBoxes :: DiffTime -> Field -> HandleSt (Mode, AbsoluteTime) IO BoxEv
handleBoxes = ((retrySt .) .) . curry . popInput . handleTimeEvPlus
	. pushInput . uncurry $ (liftHandle' .) . handle . Just
