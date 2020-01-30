{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Set

import Field
import ButtonEvent
import MonadicFrp

main :: IO ()
main = do
	f <- openField "色が変わるよ" [exposureMask, buttonPressMask]
	_ <- interpretSig (handle f) (colored f) cycleColor
	closeField f

handle :: Field -> EvReqs GUIEv -> IO (EvOccs GUIEv)
handle f r = withNextEvent f \case
	ExposeEvent {} -> flushField f >> handle f r
	ev@ButtonEvent {} -> case buttonEvent ev of
		Just BtnEvent {	buttonNumber = Button2,
				pressOrRelease = Press } -> pure . singleton . MouseDown $ Occurred [MMiddle]
		Just BtnEvent {	buttonNumber = Button3,
				pressOrRelease = Press } -> pure . singleton . MouseDown $ Occurred [MRight]
		_ -> handle f r
	_ -> handle f r

colored :: Field -> Color -> IO ()
colored f c = fillRect f (color c) 50 50 200 200 >> flushField f

color :: Color -> Pixel
color Red = 0xff0000
color Green = 0x00ff00
color Blue = 0x0000ff
color Yellow = 0xffff00
color Cyan = 0x00ffff
color Magenta = 0xff00ff
