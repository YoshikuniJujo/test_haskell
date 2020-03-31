{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.Boxes where

import Data.Bool

import TryMyInterface.Boxes.Events
import MonadicFrp.MyInterface

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = mouseDown >>= bool (clickOn b) (pure ()) . (b `elem`)

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

sameClick :: ReactG Bool
sameClick = adjust $ (==) <$> mouseDown <*> mouseDown

doubler :: ReactG ()
doubler = do
	adjust rightClick
	bool doubler (pure ()) =<< adjust (rightClick `before` sleep 0.2)
