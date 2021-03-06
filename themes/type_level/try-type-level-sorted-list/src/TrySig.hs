{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TrySig where

import Prelude hiding (repeat)

import Control.Monad.State
import Data.Time
import Data.Time.Clock.System

import Boxes
import BoxesEvents

import React
import Sig
import Handlers
import Field

tryCycleColor :: IO ()
tryCycleColor = do
	f <- openField "tryCycleColor" [exposureMask, buttonPressMask, buttonReleaseMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . print) cycleColor `runStateT` now >>= print
	closeField f

tryMousePos :: IO ()
tryMousePos = do
	f <- openField "tryMousePos" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . print) mousePos `runStateT` now >>= print
	closeField f

tryCurRect :: IO ()
tryCurRect = do
	f <- openField "tryMousePos" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . withFlush f . drawRect f 0xff0000) (curRect (200, 150))
		`runStateT` now >>= print
	closeField f

withFlush :: Field -> IO () -> IO ()
withFlush f act = clearField f >> act >> flushField f

drawRect :: Field -> Pixel -> Rect -> IO ()
drawRect f clr (Rect (l_, u_) (r_, d_)) = fillRect f clr l u w h where
	l = fromIntegral $ l_ `min` r_
	u = fromIntegral $ u_ `min` d_
	w = fromIntegral . abs $ r_ - l_
	h = fromIntegral . abs $ d_ - u_

tryDeltaTime :: IO ()
tryDeltaTime = do
	f <- openField "tryDeltaTime" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . print) (repeat $ adjust deltaTime) `runStateT` now >>= print
	closeField f

tryElapsed :: IO ()
tryElapsed = do
	f <- openField "tryElapsed" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
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
	f <- openField "tryElapsed" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . withFlush f . drawRect f 0xff0000)
		(wiggleRect $ Rect (200, 150) (400, 300)) `runStateT` now >>= print
	closeField f

tryPosInside :: IO ()
tryPosInside = do
	let	r = Rect (300, 200) (600, 400)
	f <- openField "tryPosInside" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	withFlush f $ drawRect f 0xff0000 r
	now <- systemToTAITime <$> getSystemTime
	interpret (handle 0.1 f) (posInside r mousePos) `runStateT` now >>= print
	closeField f

tryFirstPoint :: IO ()
tryFirstPoint = do
	f <- openField "tryPosInside" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpret (handle 0.1 f) firstPoint `runStateT` now >>= print
	closeField f

tryCompleteRect :: IO ()
tryCompleteRect = do
	f <- openField "tryCompleteRect" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . withFlush f . drawRect f 0xff0000)
		(completeRect (200, 150)) `runStateT` now >>= print
	closeField f

tryDefineRect :: IO ()
tryDefineRect = do
	f <- openField "tryDefineRect" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.1 f) (liftIO . withFlush f . drawRect f 0xff0000)
		defineRect `runStateT` now >>= print
	closeField f

tryChooseBoxColor :: IO ()
tryChooseBoxColor = do
	f <- openField "tryDefineRect" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . withFlush f . drawBox f)
		(chooseBoxColor (Rect (200, 150) (400, 300))) `runStateT` now >>= print
	closeField f

drawBox :: Field -> Box -> IO ()
drawBox f (Box rct clr) = drawRect f (colorToPixel clr) rct

colorToPixel :: Color -> Pixel
colorToPixel = \case
	Red -> 0xff0000
	Green -> 0x00ff00
	Blue -> 0x0000ff
	Yellow -> 0xffff00
	Cyan -> 0xff00ff
	Magenta -> 0x00ffff

tryDrClickOn :: IO ()
tryDrClickOn = do
	let	r = Rect (300, 200) (600, 400)
	f <- openField "tryDrClickOn" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	withFlush f $ drawRect f 0xff0000 r
	now <- systemToTAITime <$> getSystemTime
	interpret (handle 0.1 f) (drClickOn r) `runStateT` now >>= print
	closeField f

tryBox :: IO ()
tryBox = do
	f <- openField "tryBox" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . withFlush f . drawBox f) box
		`runStateT` now >>= print
	closeField f

tryBoxes :: IO ()
tryBoxes = do
	f <- openField "tryBox" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . withFlush f . (drawBox f `mapM_`) . reverse) boxes
		`runStateT` now >>= print
	closeField f
