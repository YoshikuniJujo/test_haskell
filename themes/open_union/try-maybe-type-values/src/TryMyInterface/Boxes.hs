{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.Boxes where

import Prelude hiding (cycle)

import Data.Bool
import Data.List.NonEmpty hiding (cycle)
import Data.List.Infinite

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

cycleColor :: SigG Color Int
cycleColor = cc (cycle $ fromList [Red .. Magenta]) 1 where
	cc :: Infinite Color -> Int -> SigG Color Int
	cc (h :~ t) i = do
		emit h
		bool (pure i) (cc t $ i + 1)
			=<< waitFor (adjust $ middleClick `before` rightClick)

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)
