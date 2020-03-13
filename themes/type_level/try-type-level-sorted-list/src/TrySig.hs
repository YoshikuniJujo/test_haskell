{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySig where

import Prelude hiding (repeat)

import Control.Monad.State
import Data.Time
import Data.Time.Clock.System

import Field
import Sig
import React
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

tryDeltaTime :: IO ()
tryDeltaTime = do
	f <- openField "tryCycleColor" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . print) (repeat $ adjust deltaTime) `runStateT` now >>= print
	closeField f

tryElapsed :: IO ()
tryElapsed = do
	f <- openField "tryCycleColor" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . drawElapsed f) elapsed `runStateT` now >>= print
	closeField f

drawElapsed :: Field -> DiffTime -> IO ()
drawElapsed f dt = do
	clearField f
	drawStr f 0x00ff00 "sans" 30 100 100 $ show dt
	flushField f

tryWiggleRect :: IO ()
tryWiggleRect = do
	f <- openField "tryCycleColor" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . drawRect f) (wiggleRect $ Rect (200, 150) (400, 300)) `runStateT` now >>= print
	closeField f

tryPosInside :: IO ()
tryPosInside = do
	let	r = Rect (300, 200) (600, 400)
	f <- openField "tryPosInside" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	drawRect f r
	now <- systemToTAITime <$> getSystemTime
	interpret (handle 0.05 f) (posInside r mousePos) `runStateT` now >>= print
	closeField f

tryFirstPoint :: IO ()
tryFirstPoint = do
	f <- openField "tryFirstPoint" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpret (handle 0.05 f) firstPoint `runStateT` now >>= print
	closeField f

tryCompleteRect :: IO ()
tryCompleteRect = do
	f <- openField "tryFirstPoint" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . drawRect f) (completeRect (200, 150)) `runStateT` now >>= print
	closeField f
