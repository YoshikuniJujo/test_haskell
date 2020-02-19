{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ColoredBoxes where

import Prelude hiding (repeat, map, scanl, until)

import Foreign.C.Types

import Signal
import React
import GuiEv
import MouseAndTime

sameClick :: ReactG s Bool
sameClick = do
	pressed <- mouseDown
	pressed2 <- mouseDown
	pure $ pressed == pressed2

clickOn :: MouseBtn -> ReactG s ()
clickOn b = do
	bs <- mouseDown
	if b `elem` bs then pure () else clickOn b

before :: ReactG s a -> ReactG s b -> ReactG s Bool
before a b = do
	(a', b') <- a `first` b
	case (done a', done b') of
		(Just _, Nothing) -> pure True
		_ -> pure False

leftClick, middleClick, rightClick :: ReactG s ()
leftClick = clickOn MLeft
middleClick = clickOn MMiddle
rightClick = clickOn MRight

releaseOf :: MouseBtn -> ReactG s ()
releaseOf b = do
	bs <- mouseUp
	if (b `elem` bs) then pure () else releaseOf b

leftUp :: ReactG s ()
leftUp = releaseOf MLeft

doubler :: ReactG s ()
doubler = do
	rightClick
	r <- rightClick `before` sleep 0.2
	if r then pure () else doubler

cycleColor :: SigG s Color Int
cycleColor = cc colors 1 where
	cc [] _ = error "never occur"
	cc (h : t) i = do
		emit h
		r <- waitFor (middleClick `before` rightClick)
		if r then cc t (i + 1) else pure i

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

colors :: [Color]
colors = cycle [Red .. Magenta]

mousePos :: SigG s Point ()
mousePos = repeat mouseMove

curRect :: Point -> SigG s Rect ()
curRect p1 = Rect p1 `map` mousePos

data Rect = Rect { leftup :: Point, rightdown :: Point } deriving Show

leftupAndSize :: Rect -> (CInt, CInt, CInt, CInt)
leftupAndSize Rect { leftup = (x1, y1), rightdown = (x2, y2) } =
	(min x1 x2, min y1 y2, abs $ x1 - x2, abs $ y1 - y2)

elapsed :: SigG s Time ()
elapsed = scanl (+) 0 $ repeat deltaTime

wiggleRect :: Rect -> SigG s Rect ()
wiggleRect (Rect lu rd) = rectAtTime `map` elapsed
	where rectAtTime t = Rect (lu +. dx) (rd +. dx)
		where dx = (sin (fromRational $ toRational t * 5) * 15.0) :: Double

(+.) :: RealFrac n => Point -> n -> Point
(x, y) +. dx = (x + round dx, y)

posInside :: Rect -> SigG s Point y -> ReactG s (Either Point y)
posInside r = find (`inside` r)

inside :: Point -> Rect -> Bool
(x, y) `inside` Rect (l, u) (r, d) = x `isBetween` (l, r) && y `isBetween` (u, d)

isBetween :: Ord a => a -> (a, a) -> Bool
isBetween x (a, b) = a <= x && x <= b || b <= x && x <= a

firstPoint :: ReactG s (Maybe Point)
firstPoint = mousePos `at` leftClick

completeRect :: Point -> SigG s Rect (Maybe Rect)
completeRect p1 = do
	(r, _) <- curRect p1 `until` leftUp
	pure $ cur r

defineRect :: SigG s Rect Rect
defineRect = waitFor firstPoint >>= \case
	Just p1 -> completeRect p1 >>= \case
		Just r -> pure r
		Nothing -> error "never occur"
	Nothing -> error "never occur"

chooseBoxColor :: Rect -> SigG s Box ()
chooseBoxColor r = () <$ always Box <^> wiggleRect r <^> cycleColor

data Box = Box Rect Color deriving Show
