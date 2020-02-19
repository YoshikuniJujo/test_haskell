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

import FieldAndMonadicFrp

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

drawColoredRect :: Field -> Color -> IO ()
drawColoredRect f c = drawRect f (Rect (100, 80) (550, 380)) c
