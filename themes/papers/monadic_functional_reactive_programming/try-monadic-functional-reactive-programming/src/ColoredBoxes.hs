{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ColoredBoxes where

import React
import GuiEv
import MouseAndTime

sameClick :: ReactG s Bool
sameClick = do
	pressed <- mouseDown
	pressed2 <- mouseDown
	pure $ pressed == pressed2

clickOn :: MouseBtn -> ReactG s ()
clickOn b = do
	bs <- mouseDown
	if b `elem` bs then pure () else clickOn b

before :: ReactG s a -> ReactG s b -> ReactG s Bool
before a b = do
	(a', b') <- a `first` b
	case (done a', done b') of
		(Just _, Nothing) -> pure True
		_ -> pure False

leftClick, middleClick, rightClick :: ReactG s ()
leftClick = clickOn MLeft
middleClick = clickOn MMiddle
rightClick = clickOn MRight

doubler :: ReactG s ()
doubler = do
	rightClick
	r <- rightClick `before` sleep 0.2
	if r then return () else doubler
