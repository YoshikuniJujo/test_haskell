{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.Boxes (
	Box(..), Rect(..), Color(..),
	leftClick, sameClick, doubler, firstPoint, cycleColor, curRect,
	elapsed, wiggleRect, completeRect, defineRect,
	chooseBoxColor, chooseBoxColor', boxes ) where

import Prelude hiding (repeat, cycle, scanl, until)

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (fromList)
import Data.List.Infinite (Infinite(..), cycle)
import Data.Time (DiffTime)
import Type.Flip ((<$%>), fpure, (<*%>))

import TryMyInterface.Boxes.Events (
	SigG, ISigG, ReactG, MouseDown, MouseUp, MouseBtn(..), Point,
	mouseDown, mouseUp, mouseMove, sleep, deltaTime )
import MonadicFrp.MyInterface (
	React, Singleton, Mergeable, Or(..), (:+:),
	adjust, first', emit, waitFor, scanl, find, repeat', spawn, parList,
	at, until', indexBy )

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = bool (clickOn b) (pure ()) . (b `elem`) =<< mouseDown

releaseOn :: MouseBtn -> React (Singleton MouseUp) ()
releaseOn b = bool (releaseOn b) (pure ()) . (b `elem`) =<< mouseUp

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

leftUp :: React (Singleton MouseUp) ()
leftUp = releaseOn MLeft

sameClick :: ReactG Bool
sameClick = adjust $ (==) <$> mouseDown <*> mouseDown

before :: Mergeable es es' => React es a -> React es' b -> React (es :+: es') Bool
l `before` r = (<$> l `first'` r) \case L _ -> True; _ -> False

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
wiggleRect (Rect lu rd) = (<$%> elapsed) \t -> let
	d = (round (sin (fromRational (toRational t) * 5) * 15 :: Double), 0) in
	Rect (lu +. d) (rd +. d)
	where (x, y) +. (dx, dy) = (x + dx, y + dy)

posInside :: Rect -> SigG Point r -> ReactG (Either Point r)
posInside rct = find (`inside` rct)
	where (x, y) `inside` Rect (l, u) (r, d) =
		(l <= x && x <= r || r <= x && x <= l) &&
		(u <= y && y <= d || d <= y && y <= u)

firstPoint :: ReactG (Maybe Point)
firstPoint = mousePos `at` leftClick

completeRect :: Point -> SigG Rect (Maybe Rect)
completeRect p1 = either Just (const Nothing) <$> curRect p1 `until'` leftUp

defineRect :: SigG Rect Rect
defineRect = waitFor firstPoint >>= \case
	Just p1 -> fromMaybe (error "never occur") <$> completeRect p1
	Nothing -> error "never occur"

chooseBoxColor, chooseBoxColor' :: Rect -> SigG Box ()
chooseBoxColor r = Box <$%> wiggleRect r <*%> (() <$ cycleColor)
chooseBoxColor' r = fpure Box <*%> wiggleRect r <*%> (() <$ cycleColor)

data Box = Box Rect Color deriving Show

drClickOn :: Rect -> ReactG (Either Point ())
drClickOn r = posInside r $ mousePos `indexBy` repeat' doubler

box :: SigG Box ()
box = () <$ do
	r <- (`Box` Red) <$%> defineRect
	chooseBoxColor r
	waitFor $ drClickOn r

newBoxes :: SigG (ISigG Box ()) ()
newBoxes = spawn box

boxes :: SigG [Box] ()
boxes = parList newBoxes
