{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Handlers where

import Data.Set
import System.Exit

import GuiEv
import React

import Field
import ButtonEvent
import FieldAndMonadicFrp

handleMotion :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handleMotion f r = withNextEvent f \case
	DestroyWindowEvent {} -> closeField f >> exitSuccess
	ev@ButtonEvent {} -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = bn,
			pressOrRelease = Press,
			position = (x, y) } -> do
			maybe (handleMotion f r)
				(pure . fromList . (MouseMove (Occurred (x, y)) :) . (: []) . MouseDown
					. Occurred . (: [])) $ mouseButton bn
		_ -> handleMotion f r
	ev@MotionEvent {} -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = ButtonX,
			pressOrRelease = Move,
			position = (x, y) } -> do
			pure . singleton . MouseMove $ Occurred (x, y)
		_ -> handleMotion f r
	ExposeEvent {} -> flushField f >> handleMotion f r
	e	| isDeleteEvent f e -> destroyField f >> handleMotion f r
		| otherwise -> print e >> handleMotion f r
