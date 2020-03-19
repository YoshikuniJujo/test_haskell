{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Boxes where

import Prelude hiding (head, map, repeat, scanl, until)
import qualified Prelude as P

import Data.Bool
import Data.List.NonEmpty (head)
import Data.Time

import Sorted
import OpenUnionValue
import Sig
import React

data MouseDown = MouseDownReq deriving Show

numbered [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccurredMouseDown [MouseBtn] deriving Show

data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = Await [inj MouseDownReq] \ev ->
	let OccurredMouseDown mbs = extract $ head ev in pure mbs

data MouseUp = MouseUpReq deriving Show

numbered [t| MouseUp |]
instance Request MouseUp where
	data Occurred MouseUp = OccurredMouseUp [MouseBtn] deriving Show

mouseUp :: React (Singleton MouseUp) [MouseBtn]
mouseUp = Await [inj MouseUpReq] \ev ->
	let OccurredMouseUp mbs = extract $ head ev in pure mbs

data MouseMove = MouseMoveReq deriving Show
type Point = (Integer, Integer)

numbered [t| MouseMove |]
instance Request MouseMove where
	data Occurred MouseMove = OccurredMouseMove Point deriving Show

mouseMove :: React (Singleton MouseMove) Point
mouseMove = Await [inj MouseMoveReq] \ev ->
	let OccurredMouseMove p = extract $ head ev in pure p

data TryWait = TryWaitReq DiffTime deriving Show

numbered [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccurredTryWait DiffTime deriving Show

tryWait :: DiffTime -> React (Singleton TryWait) DiffTime
tryWait t = Await [inj $ TryWaitReq t] \ev ->
	let OccurredTryWait t' = extract $ head ev in pure t'

sleep :: DiffTime -> React (Singleton TryWait) ()
sleep t = do
	t' <- tryWait t
	if t' == t then pure () else sleep (t - t')

data DeltaTime = DeltaTimeReq deriving Show

numbered [t| DeltaTime |]
instance Request DeltaTime where
	data Occurred DeltaTime = OccurredDeltaTime  DiffTime deriving Show

deltaTime :: React (Singleton DeltaTime) DiffTime
deltaTime = Await [inj DeltaTimeReq] \ev ->
	let OccurredDeltaTime dt = extract $ head ev in pure dt

type SigG = Sig GuiEv
type ISigG = ISig GuiEv
type ReactG = React GuiEv

type GuiEv = MouseDown :- MouseUp :- MouseMove :- TryWait :- DeltaTime :- 'Nil

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = mouseDown >>= bool (clickOn b) (pure ()) . (b `elem`)

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

releaseOn :: MouseBtn -> React (Singleton MouseUp) ()
releaseOn b = mouseUp >>= bool (releaseOn b) (pure ()) . (b `elem`)

leftUp, middleUp, rightUp :: React (Singleton MouseUp) ()
[leftUp, middleUp, rightUp] = releaseOn <$> [MLeft, MMiddle, MRight]

sameClick :: ReactG Bool
sameClick = do
	pressed <- adjust mouseDown
	pressed2 <- adjust mouseDown
	pure $ pressed == pressed2

doubler :: ReactG ()
doubler = do
	r <- adjust do
		adjust rightClick
		rightClick `before` sleep 0.2
	if r then pure () else doubler

cycleColor :: SigG Color Int
cycleColor = cc colors 1 where
	cc :: [Color] -> Int -> SigG Color Int
	cc (h : t) i = do
		emit h
		r <- waitFor . adjust $ middleClick `before` rightClick
		if r then cc t (i + 1) else pure i
	cc [] _ = error "never occur"

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

colors :: [Color]
colors = cycle [Red .. Magenta]

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
		dx = (round (sin (fromRational (toRational t) * 5) * 15 :: Double), 0)

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
chooseBoxColor r = () <$ (always Box :: SigG (Rect -> Color -> Box) ()) <^> wiggleRect r <^> cycleColor

data Box = Box Rect Color deriving Show

drClickOn :: Rect -> ReactG (Either Point ())
drClickOn r = posInside r $ mousePos `indexBy` repeat doubler

box :: SigG Box ()
box = () <$ do
	r <- (`Box` P.head colors) `map` defineRect
	chooseBoxColor r
	waitFor $ drClickOn r

newBoxes :: SigG (ISigG Box ()) ()
newBoxes = spawn box

boxes :: SigG [Box] ()
boxes = parList newBoxes
