{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.View (Box(..), Rect(..), Color(..), drawBoxes) where

import Field

data Box = Box Rect Color deriving Show
data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show
data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

type Point = (Integer, Integer)

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

withFlush :: Field -> IO () -> IO ()
withFlush f act = clearField f >> act >> flushField f

drawBoxes :: Field -> [Box] -> IO ()
drawBoxes f = withFlush f . (drawBox f `mapM_`) . reverse
