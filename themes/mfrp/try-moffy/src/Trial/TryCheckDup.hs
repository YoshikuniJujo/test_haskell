{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryCheckDup where


import Control.Monad.State
import Data.Time.Clock.System

import Moffy.React
import Moffy.React.Common
import Moffy.Event.Mouse
import Trial.Boxes.Event
import Trial.Boxes.Handle
import Trial.StepByStepBox
import Freer
import Field

tryCheckDup :: IO ()
tryCheckDup = do
	f <- openField "右クリックのあとに左クリックが2回あったら終了" [
		exposureMask, buttonPressMask, pointerMotionMask ]
	t <- systemToTAITime <$> getSystemTime
	(print =<<) . (`runStateT` t) $ runCount $ do
		cd <- addTag checkDup
		pure $ interpretReactSt InitMode (handleBoxes 0.05 f) do
			() <$ checkDup `first` checkDup
			cd `first` cd
	closeField f
				

checkDup :: ReactG s ()
checkDup = doubler >> adjust leftClick
