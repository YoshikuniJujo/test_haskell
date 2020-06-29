{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBox where

import Control.Monad.State
import Data.Type.Set
import Data.OneOrMore
import Data.Or
import Data.List.NonEmpty
import Data.List.Infinite
import Data.Time.Clock.System

import Moffy.React
import Moffy.Handle hiding (before)
import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse

import Trial.Boxes.Event
import Trial.Boxes.Handle
import Field

tryClick :: IO [MouseBtn]
tryClick = do
	f <- openField "TRY CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse Nothing f) (adjust mouseDown) <* closeField f

sameClick :: React s MouseEv Bool
sameClick = adjust do
	pressed1 <- mouseDown
	pressed2 <- mouseDown
	pure $ pressed1 == pressed2

trySameClick :: IO Bool
trySameClick = do
	f <- openField "TRY SAME CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse Nothing f) sameClick <* closeField f

leftOrRightClick :: React s MouseEv (Or () ())
leftOrRightClick = adjust $ leftClick `first` rightClick

tryLeftOrRightClick :: IO (Or () ())
tryLeftOrRightClick = do
	f <- openField "LEFT OR RIGHT CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse Nothing f) leftOrRightClick <* closeField f

leftDownRightUp :: React s MouseEv (Or () ())
leftDownRightUp = leftClick `first` rightUp

tryLeftDownRightUp :: IO (Or () ())
tryLeftDownRightUp = do
	f <- openField "LEFT DOWN RIGHT UP" [buttonPressMask, buttonReleaseMask]
	interpretReact (retry $ handleMouse Nothing f) leftDownRightUp <* closeField f

before :: (
	Update es a es' b, Expandable es (es :+: es'), Expandable es' (es :+: es'),
	Mergeable es es' (es :+: es')
	) =>
	React s es a -> React s es' b -> React s (es :+: es') Bool
a `before` b = a `first` b >>= \case
	L _ -> pure True
	_ -> pure False

doubler :: ReactG s ()
doubler = do
	adjust rightClick
	r <- adjust (rightClick `before` sleep 0.2)
	if r then pure () else doubler

tryDoubler :: IO ()
tryDoubler = do
	f <- openField "TRY DOUBLER" [buttonPressMask]
	void . (interpretReactSt InitMode (handleBoxes 0.05 f) doubler `runStateT`) . systemToTAITime =<< getSystemTime
	closeField f

data Color = Read | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

-- cycleColor
