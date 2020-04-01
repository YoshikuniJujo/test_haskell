{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.Boxes (
	Box(..), Rect(..), Color(..),
	leftClick, sameClick, doubler, firstPoint, cycleColor, curRect,
	elapsed, wiggleRect, completeRect, defineRect,
	chooseBoxColor, chooseBoxColor', boxes ) where

import Prelude hiding (repeat, cycle, scanl, until)

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

releaseOn :: MouseBtn -> React (Singleton MouseUp) ()
releaseOn b = bool (releaseOn b) (pure ()) . (b `elem`) =<< mouseUp

leftUp :: React (Singleton MouseUp) ()
leftUp = releaseOn MLeft

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
mousePos = repeat' $ adjust mouseMove

curRect :: Point -> SigG Rect ()
curRect p1 = Rect p1 <$%> mousePos

data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show

elapsed :: SigG DiffTime ()
elapsed = scanl (+) 0 . repeat' $ adjust deltaTime

wiggleRect :: Rect -> SigG Rect ()
wiggleRect (Rect lu rd) = rectAtTime <$%> elapsed where
	rectAtTime t = Rect (lu +. dx) (rd +. dx) where
		dx = (	round (sin (fromRational (toRational t) * 5) * 15
				:: Double),
			0 )

(+.) :: Point -> Point -> Point
(x1, y1) +. (x2, y2) = (x1 + x2, y1 + y2)

posInside :: Rect -> SigG Point r -> ReactG (Either Point r)
posInside r = find (`inside` r)

inside :: Point -> Rect -> Bool
(x, y) `inside` Rect (l, u) (r, d) =
	(l <= x && x <= r || r <= x && x <= l) &&
	(u <= y && y <= d || d <= y && y <= u)

firstPoint :: ReactG (Maybe Point)
firstPoint = mousePos `at` leftClick

completeRect :: Point -> SigG Rect (Maybe Rect)
completeRect p1 = (<$> curRect p1 `until'` leftUp) \case
	Left rct -> Just rct
	Right _ -> Nothing

defineRect :: SigG Rect Rect
defineRect = waitFor firstPoint >>= \case
	Just p1 -> completeRect p1 >>= \case
		Just r -> pure r
		Nothing -> error "never occur"
	Nothing -> error "never occur"

chooseBoxColor, chooseBoxColor' :: Rect -> SigG Box ()
chooseBoxColor r = Box <$%> wiggleRect r <*%> (() <$ cycleColor)
chooseBoxColor' r = fpure Box <*%> wiggleRect r <*%> (() <$ cycleColor)

data Box = Box Rect Color deriving Show

drClickOn :: Rect -> ReactG (Either Point ())
drClickOn r = posInside r (mousePos `indexBy` repeat' doubler)

box :: SigG Box ()
box = () <$ do
	r <- (`Box` Red) <$%> defineRect
	chooseBoxColor r
	waitFor $ drClickOn r

newBoxes :: SigG (ISigG Box ()) ()
newBoxes = spawn box

boxes :: SigG [Box] ()
boxes = parList newBoxes
