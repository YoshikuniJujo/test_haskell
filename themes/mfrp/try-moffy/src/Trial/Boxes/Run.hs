{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Run (runBoxes) where

import Prelude hiding (cycle, repeat, scanl, until, break)

import Control.Moffy.Run
import Data.Time.Clock.System

import Field hiding (Point)

import Trial.Boxes.Handle
import Trial.Boxes.View

runBoxes :: String -> SigG s [Box] r -> IO r
runBoxes ttl sig = do
	f <- openField ttl [
		pointerMotionMask, buttonPressMask, buttonReleaseMask,
		exposureMask ]
	(r, _) <- interpretSt (handleBoxes 0.05 f) (drawBoxes f) sig . (InitialMode ,)
			. systemToTAITime =<< getSystemTime
	r <$ closeField f
