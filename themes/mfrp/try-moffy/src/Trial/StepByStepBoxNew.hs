{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBoxNew where

import Prelude hiding (cycle)

import Control.Monad.State
import Data.Type.Set
import Data.OneOrMore
import Data.Bool
import Data.Or
import Data.List.NonEmpty hiding (cycle, repeat, scanl)
import Data.List.Infinite hiding (repeat, scanl)
import Data.Time.Clock.System

import Moffy.ReactNew
import Moffy.React.Common
import Moffy.Sig.Common
import Moffy.Handle hiding (before)
import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse
import Field

import Trial.Boxes.Event
import Trial.Boxes.Handle

tryClickNew :: IO [MouseBtn]
tryClickNew = do
	f <- openField "TRY CLICK" [buttonPressMask, exposureMask]
	interpretReact (retry $ handleMouse Nothing f) (adjust mouseDown) <* closeField f

sameClickNew :: React s MouseEv Bool
sameClickNew = adjust $ (==) <$> mouseDown <*> mouseDown

trySameClickNew :: IO Bool
trySameClickNew = do
	f <- openField "TRY SAME CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse Nothing f) sameClickNew <* closeField f

leftDownRightUpNew :: React s MouseEv (Or () ())
leftDownRightUpNew = adjust $ leftClick `first` rightUp

tryLeftDownRightUpNew :: IO (Or () ())
tryLeftDownRightUpNew = do
	f <- openField "LEFT DOWN RIGHT UP" [buttonPressMask, buttonReleaseMask]
	interpretReact (retry $ handleMouse Nothing f) leftDownRightUpNew <* closeField f

before :: (
	Update a b,
	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es',
	Expandable es (es :+: es'), Expandable es' (es :+: es'),
	Mergeable (es :+: es') (es :+: es') (es :+: es')
	) => React s es a -> React s es' b -> React s (es :+: es') Bool
l `before` r = l `first` r >>= \case L _ -> pure True; _ -> pure False

doubler :: ReactG s ()
doubler = do
	adjust rightClick
	bool doubler (pure ()) =<< adjust (rightClick `before` sleep 0.2)

tryDoublerNew :: IO ()
tryDoublerNew = do
	f <- openField "TRY DOUBLER" [buttonPressMask, exposureMask]
	void . (interpretReactSt InitMode (handleBoxes 0.05 f) doubler `runStateT`) . systemToTAITime =<< getSystemTime
	closeField f

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

cycleColor :: SigG s Color ()
cycleColor = cc . cycle $ fromList [Red .. Magenta] where
	cc (h :~ t) = emit h >>
		(bool (pure ()) (cc t)
			=<< waitFor (adjust $ middleClick `before` rightClick))

tryCycleColorNew :: IO ()
tryCycleColorNew = do
	f <- openField "TRY CYCLE COLOR" [buttonPressMask, exposureMask]
	void . (interpretSt InitMode (handleBoxes 0.05 f) (liftIO . print) cycleColor `runStateT`) . systemToTAITime =<< getSystemTime
	closeField f
