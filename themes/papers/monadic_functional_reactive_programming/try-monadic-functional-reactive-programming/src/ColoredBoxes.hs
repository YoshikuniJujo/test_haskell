{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ColoredBoxes where

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
