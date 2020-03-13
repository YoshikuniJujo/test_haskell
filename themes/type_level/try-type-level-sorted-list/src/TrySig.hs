{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySig where

import Control.Monad.State
import Data.Time.Clock.System

import Field
import Sig
import Handlers

tryCycleColor :: IO ()
tryCycleColor = do
	f <- openField "tryCycleColor" [exposureMask, buttonPressMask, buttonReleaseMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . print) cycleColor `runStateT` now >>= print
	closeField f

tryMousePos :: IO ()
tryMousePos = do
	f <- openField "tryCycleColor" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . print) mousePos `runStateT` now >>= print
	closeField f

tryCurRect :: IO ()
tryCurRect = do
	f <- openField "tryCycleColor" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . drawRect f) (curRect (100, 100)) `runStateT` now >>= print
	closeField f

drawRect :: Field -> Rect -> IO ()
drawRect f (Rect (l_, u_) (r_, d_)) = do
	clearField f
	fillRect f 0xff0000 l u w h
	flushField f
	where
	l = fromIntegral $ l_ `min` r_
	u = fromIntegral $ u_ `min` d_
	w = fromIntegral . abs $ r_ - l_
	h = fromIntegral . abs $ d_ - u_
