{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Boxes where

import Prelude hiding (cycle)

import Data.Bool
import Data.List.NonEmpty hiding (cycle)

import BoxesEvents
import Sig
import React
import Sorted
import Infinite

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

cycleColor :: SigG Color Int
cycleColor = cc (cycle $ fromList [Red .. Magenta]) 1 where
	cc :: Infinite Color -> Int -> SigG Color Int
	cc (h :~ t) i = do
		emit h
		r <- waitFor . adjust $ middleClick `before` rightClick
		if r then cc t (i + 1) else pure i

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)
