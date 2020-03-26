{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySig where

import Control.Monad.State
import Data.Time.Clock.System

import Boxes
import Handlers
import Sig
import Field

tryCycleColor :: IO ()
tryCycleColor = do
	f <- openField "tryCycleColor" [exposureMask, buttonPressMask, buttonReleaseMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . print) cycleColor `runStateT` now >>= print
	closeField f

tryMousePos :: IO ()
tryMousePos = do
	f <- openField "tryMousePos" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . print) mousePos `runStateT` now >>= print
	closeField f

tryCurRect :: IO ()
tryCurRect = do
	f <- openField "tryCurRect" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . withFlush f . drawRect f 0xff0000)
		(curRect (300, 200)) `runStateT` now >>= print
	closeField f

withFlush :: Field -> IO () -> IO ()
withFlush f act = clearField f >> act >> flushField f

drawRect :: Field -> Pixel -> Rect -> IO ()
drawRect f clr (Rect (l_, u_) (r_, d_)) = fillRect f clr l u w h where
	l = fromIntegral $ l_ `min` r_
	u = fromIntegral $ u_ `min` d_
	w = fromIntegral . abs $ r_ - l_
	h = fromIntegral . abs $ d_ - u_
