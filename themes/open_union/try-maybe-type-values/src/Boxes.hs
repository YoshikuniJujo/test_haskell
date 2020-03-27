{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Boxes where

import Prelude hiding (map, repeat, cycle, scanl, until)

import Data.Bool
import Data.List.NonEmpty hiding (map, repeat, cycle, scanl)
import Data.Time

import BoxesEvents
import Sig
import React
import Sorted
import Infinite

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = mouseDown >>= bool (clickOn b) (pure ()) . (b `elem`)

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

releaseOn :: MouseBtn -> React (Singleton MouseUp) ()
releaseOn b = mouseUp >>= bool (releaseOn b) (pure ()) . (b `elem`)

leftUp, middleUp, rightUp :: React (Singleton MouseUp) ()
[leftUp, middleUp, rightUp] = releaseOn <$> [MLeft, MMiddle, MRight]

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

mousePos :: SigG Point ()
mousePos = repeat $ adjust mouseMove

curRect :: Point -> SigG Rect ()
curRect p1 = Rect p1 `map` mousePos

data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show

elapsed :: SigG DiffTime ()
elapsed = scanl (+) 0 . repeat $ adjust deltaTime

wiggleRect :: Rect -> SigG Rect ()
wiggleRect (Rect lu rd) = rectAtTime `map` elapsed where
	rectAtTime t = Rect (lu +. dx) (rd +. dx) where
		dx = (	round (sin (fromRational (toRational t) * 5) * 15
				:: Double),
			0 )

(+.) :: Point -> Point -> Point
(x1, y1) +. (x2, y2) = (x1 + x2, y1 + y2)

posInside :: Rect -> SigG Point y -> ReactG (Either Point y)
posInside r = find (`inside` r)

inside :: Point -> Rect -> Bool
inside (x, y) (Rect (l, u) (r, d)) =
	(l <= x && x <= r || r <= x && x <= l) &&
	(u <= y && y <= d || d <= y && y <= u)

firstPoint :: ReactG (Maybe Point)
firstPoint = mousePos `at` leftClick

completeRect :: Point -> SigG Rect (Maybe Rect)
completeRect p1 = do
	(r, _) <- curRect p1 `until` leftUp
	pure $ cur r

defineRect :: SigG Rect Rect
defineRect = waitFor firstPoint >>= \case
	Just p1 -> completeRect p1 >>= \case
		Just r -> pure r
		Nothing -> error "never occur"
	Nothing -> error "never occur"

chooseBoxColor :: Rect -> SigG Box ()
chooseBoxColor r = () <$ (always Box :: SigG (Rect -> Color -> Box) ()) <^> wiggleRect r <^> cycleColor

data Box = Box Rect Color deriving Show
