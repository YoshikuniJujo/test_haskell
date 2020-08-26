{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBox (tryBoxes) where

import Prelude hiding (cycle, repeat, scanl, until, break)

import Control.Moffy
import Control.Moffy.Run
import Data.Time.Clock.System

import Control.Moffy.Event.Delete
import Field hiding (Point)

import Trial.Boxes
import Trial.Boxes.Event
import Trial.Boxes.Handle
import Trial.Boxes.View

trySigGBoxes' :: String -> SigG s [Box] r -> IO r
trySigGBoxes' ttl sig = do
	f <- openField ttl [
		pointerMotionMask, buttonPressMask, buttonReleaseMask,
		exposureMask ]
	(r, _) <- interpretSt (handleBoxes 0.05 f) (drawBoxes f) sig . (InitialMode ,)
			. systemToTAITime =<< getSystemTime
	r <$ closeField f

tryBoxes :: IO ()
tryBoxes = () <$ trySigGBoxes' "TRY BOXES" (boxes `break` deleteEvent)
