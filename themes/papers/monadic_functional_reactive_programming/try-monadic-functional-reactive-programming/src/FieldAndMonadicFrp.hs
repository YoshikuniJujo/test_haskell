{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FieldAndMonadicFrp where

import ColoredBoxes
import GuiEv
import Field
import ButtonEvent

mouseButton :: ButtonNumber -> Maybe MouseBtn
mouseButton Button1 = Just MLeft
mouseButton Button2 = Just MMiddle
mouseButton Button3 = Just MRight
mouseButton _ = Nothing

drawRect :: Field -> Rect -> Color -> IO ()
drawRect f rct clr = do
	clearField f
	fillRect f (colorToPixel clr) l u w h
	flushField f
	where
	(l_, u_, w_, h_) = leftupAndSize rct
	[l, u] = fromIntegral <$> [l_, u_]
	[w, h] = fromIntegral <$> [w_, h_]

colorToPixel :: Color -> Pixel
colorToPixel Red = 0xff0000
colorToPixel Green = 0x00ff00
colorToPixel Blue = 0x0000ff
colorToPixel Yellow = 0xffff00
colorToPixel Cyan = 0x00ffff
colorToPixel Magenta = 0xff00ff

drawBox :: Field -> Box -> IO ()
drawBox f (Box rct clr) = drawRect f rct clr
