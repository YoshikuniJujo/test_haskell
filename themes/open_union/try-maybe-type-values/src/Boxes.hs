{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Boxes where

import Data.Bool

import BoxesEvents
import React
import Sorted

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = mouseDown >>= bool (clickOn b) (pure ()) . (b `elem`)

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

sameClick :: ReactG Bool
sameClick = adjust do
	pressed <- mouseDown
	pressed2 <- mouseDown
	pure $ pressed == pressed2

doubler :: ReactG ()
doubler = adjust do
	r <- adjust do
		adjust rightClick
		rightClick `before` sleep 0.2
	if r then pure () else doubler
