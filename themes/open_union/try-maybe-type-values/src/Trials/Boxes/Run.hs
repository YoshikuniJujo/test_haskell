{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.Run (withInterpretSig, withFlush, drawBox, drawRect, drawElapsed) where

import Control.Monad.State (runStateT, liftIO)
import Data.Time (DiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)

import Trials.Boxes.View (Box(..), Rect(..), Color(..))
import Trials.Boxes.Handle (SigG, handle, handle', handleSt, AB(..))
import MonadicFrp.Run (interpret, interpretSt)
import Field (
	Field, Pixel,
	openField, closeField, flushField, clearField, fillRect, drawStr,
	exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask )

withInterpretSig ::
	Show r => String -> (Field -> a -> IO ()) -> SigG a r -> IO ()
withInterpretSig fn op s = do
	f <- openField fn [
		exposureMask, buttonPressMask,
		buttonReleaseMask, pointerMotionMask ]
	now <- systemToTAITime <$> getSystemTime
--	print =<< interpret (handle' 0.05 f) (liftIO . op f) s `runStateT` now
	print =<< interpretSt A (handleSt 0.05 f) (liftIO . op f) s `runStateT` now
	closeField f

withFlush :: Field -> IO () -> IO ()
withFlush f act = clearField f >> act >> flushField f

drawElapsed :: Field -> DiffTime -> IO ()
drawElapsed f dt = withFlush f . drawStr f 0x00ff00 "sans" 30 100 100 $ show dt

drawRect :: Field -> Pixel -> Rect -> IO ()
drawRect f clr (Rect (l_, u_) (r_, d_)) = fillRect f clr l u w h where
	l = fromIntegral $ l_ `min` r_
	u = fromIntegral $ u_ `min` d_
	w = fromIntegral . abs $ r_ - l_
	h = fromIntegral . abs $ d_ - u_

drawBox :: Field -> Box -> IO ()
drawBox f (Box rct clr) = drawRect f (colorToPixel clr) rct

colorToPixel :: Color -> Pixel
colorToPixel = \case
	Red -> 0xff0000; Green -> 0x00ff00; Blue -> 0x0000ff
	Yellow -> 0xffff00; Cyan -> 0xff00ff; Magenta -> 0x00ffff
