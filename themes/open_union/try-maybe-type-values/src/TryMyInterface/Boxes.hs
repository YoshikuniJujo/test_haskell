{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.Boxes where

import Prelude hiding (repeat, cycle, scanl)

import Data.Bool
import Data.List.NonEmpty hiding (repeat, cycle, scanl)
import Data.List.Infinite
import Data.Time

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

mousePos :: SigG Point ()
mousePos = repeat $ adjust mouseMove

curRect :: Point -> SigG Rect ()
curRect p1 = Rect p1 <$%> mousePos

data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show

elapsed :: SigG DiffTime ()
elapsed = scanl (+) 0 . repeat $ adjust deltaTime

wiggleRect :: Rect -> SigG Rect ()
wiggleRect (Rect lu rd) = rectAtTime <$%> elapsed where
	rectAtTime t = Rect (lu +. dx) (rd +. dx) where
		dx = (	round (sin (fromRational (toRational t) * 5) * 15
				:: Double),
			0 )

(+.) :: Point -> Point -> Point
(x1, y1) +. (x2, y2) = (x1 + x2, y1 + y2)
