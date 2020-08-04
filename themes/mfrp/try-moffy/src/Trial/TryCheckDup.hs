{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryCheckDup where

import Control.Moffy
import Control.Moffy.Run
import Data.Type.Set
import Data.Time.Clock.System

import Control.Moffy.Event.Mouse
import Trial.Boxes.Event
import Trial.Boxes.Handle
import Trial.StepByStepBox
import Control.Monad.Freer.Par
import Field

tryCheckDup :: IO ()
tryCheckDup = do
	f <- openField "右クリックのあとに左クリックが2回あったら終了" [
		exposureMask, buttonPressMask, pointerMotionMask ]
	t <- systemToTAITime <$> getSystemTime
	print =<< runUnique do
		cd <- tag checkDup
		pure . ($ (InitMode, t)) $ interpretReactSt (handleBoxes' 0.05 f) do
			() <$ checkDup `first` checkDup
			cd `first` cd
	closeField f
				

checkDup :: React s (MouseDown :- TryWait :- 'Nil) ()
checkDup = doubler >> adjust leftClick
