{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBoxNew where

import Prelude hiding (cycle, repeat)

import Control.Monad.State
import Data.Type.Set
import Data.Type.Flip
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
import Field hiding (Point)

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

mousePos :: SigG s Point ()
mousePos = repeat $ adjust mouseMove

tryMousePosNew :: IO ()
tryMousePosNew = do
	f <- openField "TRY MOUSE POS" [pointerMotionMask, exposureMask]
	void . (interpretSt InitMode (handleBoxes 0.05 f) (liftIO . print) mousePos `runStateT`) . systemToTAITime =<< getSystemTime
	closeField f

curRect :: Point -> SigG s Rect ()
curRect p1 = Rect p1 <$%> mousePos

tryCurRectNew :: IO ()
tryCurRectNew = trySigGRect "TRY CUR RECT" $ curRect (200, 150)

data Rect = Rect { leftup :: Point, rightdown :: Point  }

trySigGRect :: String -> SigG s Rect r -> IO r
trySigGRect ttl sig = do
	f <- openField ttl [
		pointerMotionMask, buttonPressMask, buttonReleaseMask,
		exposureMask ]
	(r, _) <- (interpretSt InitMode (handleBoxes 0.05 f)
				(liftIO . withFlush f . drawRect f (colorToPixel Red)) sig `runStateT`)
			. systemToTAITime =<< getSystemTime
	r <$ closeField f

withFlush :: Field -> IO a -> IO a
withFlush f act = clearField f >> act <* flushField f

drawRect :: Field -> Pixel -> Rect -> IO ()
drawRect f clr (Rect (l_, u_) (r_, d_)) = fillRect f clr l u w h where
	[l, u] = fromIntegral <$> [l_ `min` r_, u_ `min` d_]
	[w, h] = fromIntegral <$> [abs $ r_ - l_, abs $ d_ - u_]


colorToPixel :: Color -> Pixel
colorToPixel = \case
	Red -> 0xff0000; Green -> 0x00ff00; Blue -> 0x0000ff
	Yellow -> 0xffff00; Cyan -> 0xff00ff; Magenta -> 0x00ffff
