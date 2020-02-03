{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Set

import Field
import ButtonEvent
import MonadicFrp
import ColorToPixel

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
