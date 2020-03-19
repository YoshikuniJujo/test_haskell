{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Boxes where

import Prelude hiding (head, tail, map, repeat, cycle, scanl, until)

import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Time (DiffTime)

import Sig (
	cur, emit, always, waitFor,
	map, scanl, find, repeat, spawn, parList, at, until, (<^>), indexBy )
import React (React, adjust, before)
import Sorted (Singleton)
import Infinite (Infinite(..), head, cycle)

import BoxesEvents (
	SigG, ReactG, MouseDown, MouseUp, MouseBtn(..), Point,
	mouseDown, mouseUp, mouseMove, sleep, deltaTime )

---------------------------------------------------------------------------

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = mouseDown >>= bool (clickOn b) (pure ()) . (b `elem`)

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

releaseOn :: MouseBtn -> React (Singleton MouseUp) ()
releaseOn b = mouseUp >>= bool (releaseOn b) (pure ()) . (b `elem`)

leftUp, middleUp, rightUp :: React (Singleton MouseUp) ()
[leftUp, middleUp, rightUp] = releaseOn <$> [MLeft, MMiddle, MRight]

sameClick :: ReactG Bool
sameClick = adjust $ (==) <$> mouseDown <*> mouseDown

doubler :: ReactG ()
doubler = bool doubler (pure ())
	=<< adjust (adjust rightClick >> (rightClick `before` sleep 0.2))

cycleColor :: SigG Color Int
cycleColor = cc colors 1 where
	cc :: Infinite Color -> Int -> SigG Color Int
	cc (h :~ t) i = do
		emit h
		bool (pure i) (cc t (i + 1))
			=<< waitFor (adjust $ middleClick `before` rightClick)

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

colors :: Infinite Color
colors = cycle $ Red :| [Green .. Magenta]

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
completeRect p1 = cur . fst <$> curRect p1 `until` leftUp

defineRect :: SigG Rect Rect
defineRect = waitFor firstPoint >>= \case
	Just p1 -> completeRect p1 >>= \case
		Just r -> pure r
		Nothing -> error "bad"
	Nothing -> error "bad"

chooseBoxColor :: Rect -> SigG Box ()
chooseBoxColor r = () <$ (always Box :: SigG (Rect -> Color -> Box) ())
	<^> wiggleRect r <^> cycleColor

data Box = Box Rect Color deriving Show

drClickOn :: Rect -> ReactG (Either Point ())
drClickOn r = posInside r $ mousePos `indexBy` repeat doubler

box :: SigG Box ()
box = () <$ do
	r <- (`Box` head colors) `map` defineRect
	chooseBoxColor r
	waitFor $ drClickOn r

boxes :: SigG [Box] ()
boxes = parList newBoxes where newBoxes = spawn box
