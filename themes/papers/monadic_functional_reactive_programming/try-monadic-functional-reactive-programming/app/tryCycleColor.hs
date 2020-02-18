{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Set

import ColoredBoxes
import GuiEv
import Signal
import React
import Field
import ButtonEvent

main :: IO ()
main = do
	f <- openField "中クリックで色が変わるよ"
		[exposureMask, buttonPressMask]
	interpretSig (handle f) (drawColoredRect f) cycleColor >>= print
	closeField f

handle :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handle f r = withNextEvent f \case
	ev@ButtonEvent {} -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = bn,
			pressOrRelease = Press,
			position = (_x, _y) } -> do
			maybe (handle f r)
				(pure . singleton . MouseDown
					. Occurred . (: [])) $ mouseButton bn
		_ -> handle f r
	ExposeEvent {} -> flushField f >> handle f r
	e -> print e >> handle f r

mouseButton :: ButtonNumber -> Maybe MouseBtn
mouseButton Button1 = Just MLeft
mouseButton Button2 = Just MMiddle
mouseButton Button3 = Just MRight
mouseButton _ = Nothing

drawColoredRect :: Field -> Color -> IO ()
drawColoredRect f c = fillRect f (colorToPixel c) 100 80 450 300 >> flushField f

colorToPixel :: Color -> Pixel
colorToPixel Red = 0xff0000
colorToPixel Green = 0x00ff00
colorToPixel Blue = 0x0000ff
colorToPixel Yellow = 0xffff00
colorToPixel Cyan = 0x00ffff
colorToPixel Magenta = 0xff00ff
