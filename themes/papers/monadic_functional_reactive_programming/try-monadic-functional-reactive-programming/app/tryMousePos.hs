{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Set

import ColoredBoxes
import Signal
import GuiEv
import React

import Field
import ButtonEvent

import FieldAndMonadicFrp

main :: IO ()
main = do
	f <- openField "マウスの動き" [
		exposureMask, buttonPressMask, button1MotionMask ]
	interpretSig (handle f) print mousePos
	closeField f

handle :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handle f r = withNextEvent f \case
	ev@ButtonEvent {} -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = bn,
			pressOrRelease = Press,
			position = (x, y) } -> do
			maybe (handle f r)
				(pure . fromList . (MouseMove (Occurred (x, y)) :) . (: []) . MouseDown
					. Occurred . (: [])) $ mouseButton bn
		_ -> handle f r
	ev@MotionEvent {} -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = ButtonX,
			pressOrRelease = Move,
			position = (x, y) } -> do
			pure . singleton . MouseMove $ Occurred (x, y)
		_ -> handle f r
	ExposeEvent {} -> flushField f >> handle f r
	e -> print e >> handle f r
