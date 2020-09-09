{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.View (
	-- * drawBoxes
	Box, drawBoxes ) where

import Trial.Boxes.Box (Box(..), Rect(..), Color(..))
import Field (Field, Pixel, flushField, clearField, fillRect)

---------------------------------------------------------------------------

drawBoxes :: Field -> [Box] -> IO ()
drawBoxes f = withFlush f . (drawBox f `mapM_`)

drawBox :: Field -> Box -> IO ()
drawBox f (Box rct clr) = drawRect f (clrToPx clr) rct
	where clrToPx = \case
		Red -> 0xff0000; Green -> 0x00ff00; Blue -> 0x0000ff
		Yellow -> 0xffff00; Cyan -> 0xff00ff; Magenta -> 0x00ffff

drawRect :: Field -> Pixel -> Rect -> IO ()
drawRect f clr (Rect (l_, u_) (r_, d_)) = fillRect f clr l u w h where
	[l, u] = round <$> [l_ `min` r_, u_ `min` d_]
	[w, h] = round <$> [abs $ r_ - l_, abs $ d_ - u_]

withFlush :: Field -> IO a -> IO a
withFlush f act = clearField f >> act <* flushField f
