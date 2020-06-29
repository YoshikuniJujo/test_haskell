{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBox where

import Prelude hiding (cycle, repeat)

import Control.Monad.State
import Data.Type.Flip
import Data.Type.Set
import Data.OneOrMore
import Data.Or
import Data.Bool
import Data.List.NonEmpty hiding (cycle, repeat)
import Data.List.Infinite hiding (repeat)
import Data.Time.Clock.System

import Moffy.React
import Moffy.Sig
import Moffy.Handle hiding (before)
import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse

import Trial.Boxes.Event
import Trial.Boxes.Handle
import Field hiding (Point)

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
leftDownRightUp = adjust $ leftClick `first` rightUp

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

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

cycleColor :: SigG s Color ()
cycleColor = cc . cycle $ fromList [Red .. Magenta] where
	cc (h :~ t) = emit h >>
		(bool (pure ()) (cc t)
			=<< waitFor (adjust $ middleClick `before` rightClick))

tryCycleColor :: IO ()
tryCycleColor = do
	f <- openField "TRY CYCLE COLOR" [buttonPressMask]
	void . (interpretSt InitMode (handleBoxes 0.05 f) (liftIO . print) cycleColor `runStateT`) . systemToTAITime =<< getSystemTime
	closeField f

-- type Point = (Integer, Integer)

mousePos :: SigG s Point ()
mousePos = repeat $ adjust mouseMove

tryMousePos :: IO ()
tryMousePos = do
	f <- openField "TRY MOUSE POS" [pointerMotionMask]
	void . (interpretSt InitMode (handleBoxes 0.05 f) (liftIO . print) mousePos `runStateT`) . systemToTAITime =<< getSystemTime	
	closeField f

curRect :: Point -> SigG s Rect ()
curRect p1 = Rect p1 <$%> mousePos

data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show

tryCurRect :: IO ()
tryCurRect = do
	f <- openField "TRY CUR RECT" [pointerMotionMask]
	void . (interpretSt InitMode (handleBoxes 0.05 f) (liftIO . print) (curRect (150, 100)) `runStateT`)
		. systemToTAITime =<< getSystemTime
	closeField f
