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

drawRect :: Field -> Rect -> IO ()
drawRect f rct = do
	clearField f
	fillRect f 0xff0000 l u w h
	flushField f
	where
	(l_, u_, w_, h_) = leftupAndSize rct
	[l, u] = fromIntegral <$> [l_, u_]
	[w, h] = fromIntegral <$> [w_, h_]
