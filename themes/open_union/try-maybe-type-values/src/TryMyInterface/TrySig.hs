{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.TrySig where

import Control.Monad.State
import Data.Time
import Data.Time.Clock.System

import TryMyInterface.Boxes
import TryMyInterface.Boxes.Events
import TryMyInterface.Boxes.Handlers
import MonadicFrp.MyInterface
import Field

withInterpretSig :: Show r => String -> (Field -> a -> IO ()) -> SigG a r -> IO ()
withInterpretSig fn op s = do
	f <- openField fn [
		exposureMask, buttonPressMask,
		buttonReleaseMask, pointerMotionMask ]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . op f) s `runStateT` now >>= print
	closeField f

tryCycleColor :: IO ()
tryCycleColor = withInterpretSig "tryCycleColor" (const print) cycleColor

tryCurRect :: IO ()
tryCurRect = withInterpretSig "tryCurRect"
	(\f -> withFlush f . drawRect f 0xff0000) $ curRect (300, 200)

withFlush :: Field -> IO () -> IO ()
withFlush f act = clearField f >> act >> flushField f

drawRect :: Field -> Pixel -> Rect -> IO ()
drawRect f clr (Rect (l_, u_) (r_, d_)) = fillRect f clr l u w h where
	l = fromIntegral $ l_ `min` r_
	u = fromIntegral $ u_ `min` d_
	w = fromIntegral . abs $ r_ - l_
	h = fromIntegral . abs $ d_ - u_

tryElapsed :: IO ()
tryElapsed = withInterpretSig "tryElapsed" drawElapsed elapsed

drawElapsed :: Field -> DiffTime -> IO ()
drawElapsed f dt = do
	clearField f
	drawStr f 0x00ff00 "sans" 30 100 100 $ show dt
	flushField f
