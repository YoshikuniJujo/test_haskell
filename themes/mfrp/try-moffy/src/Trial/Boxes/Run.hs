{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Run (trySigGBoxes') where

import Prelude hiding (cycle, repeat, scanl, until, break)

import Control.Moffy.Run
import Data.Time.Clock.System

import Field hiding (Point)

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
