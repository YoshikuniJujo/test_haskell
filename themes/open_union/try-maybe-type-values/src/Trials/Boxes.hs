{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes (boxes) where

import Prelude hiding (repeat, cycle, scanl, until)

import Data.Type.Flip ((<$%>), (<*%>))
import Data.Type.Set (Singleton, (:+:))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Or (Or(..))
import Data.List.NonEmpty (fromList)
import Data.List.Infinite (Infinite(..), cycle)
import Data.Time (DiffTime)

import Trials.Boxes.Event (
	SigG, ISigG, ReactG, MouseDown, MouseUp, MouseBtn(..), Point,
	mouseDown, mouseUp, mouseMove, sleep, deltaTime )
import Trials.Boxes.View
import MonadicFrp (
	React, Firstable,
	adjust, first, emit, waitFor, scanl, find, repeat, spawn, parList,
	at, until, indexBy )

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = bool (clickOn b) (pure ()) . (b `elem`) =<< mouseDown

releaseOn :: MouseBtn -> React (Singleton MouseUp) ()
releaseOn b = bool (releaseOn b) (pure ()) . (b `elem`) =<< mouseUp

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

leftUp :: React (Singleton MouseUp) ()
leftUp = releaseOn MLeft

before :: Firstable es es' => React es a -> React es' b -> React (es :+: es') Bool
l `before` r = (<$> l `first` r) \case L _ -> True; _ -> False

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

mousePos :: SigG Point ()
mousePos = repeat $ adjust mouseMove

curRect :: Point -> SigG Rect ()
curRect p1 = Rect p1 <$%> mousePos

elapsed :: SigG DiffTime ()
elapsed = scanl (+) 0 . repeat $ adjust deltaTime

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

firstPoint :: ReactG (Either (Point, ()) (Maybe ()))
firstPoint = mousePos `at` leftClick

completeRect :: Point -> SigG Rect (Maybe Rect)
completeRect p1 = either (Just . fst) (const Nothing) <$> curRect p1 `until` leftUp

defineRect :: SigG Rect Rect
defineRect = waitFor firstPoint >>= \case
	Left (p1, _) -> fromMaybe (error "never occur") <$> completeRect p1
	Right _ -> error "never occur"

chooseBoxColor :: Rect -> SigG Box ()
chooseBoxColor r = Box <$%> wiggleRect r <*%> (() <$ cycleColor)

drClickOn :: Rect -> ReactG (Either Point (Either (Point, ()) (Maybe ())))
drClickOn r = posInside r $ mousePos `indexBy` repeat doubler

box :: SigG Box ()
box = () <$ do
	r <- (`Box` Red) <$%> defineRect
	chooseBoxColor r
	waitFor $ drClickOn r

newBoxes :: SigG (ISigG Box ()) ()
newBoxes = spawn box

boxes :: SigG [Box] ()
boxes = parList newBoxes
