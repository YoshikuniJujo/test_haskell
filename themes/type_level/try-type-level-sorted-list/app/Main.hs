{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Time.Clock.System

import Boxes
import Handlers
import Sig
import Field

main :: IO ()
main = tryBoxes

tryBoxes :: IO ()
tryBoxes = do
	f <- openField "tryBox" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	now <- systemToTAITime <$> getSystemTime
	interpretSig (handle 0.05 f) (liftIO . withFlush f . (drawBox f `mapM_`) . reverse) boxes
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
