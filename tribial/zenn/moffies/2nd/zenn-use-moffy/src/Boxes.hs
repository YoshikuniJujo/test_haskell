{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Boxes where

import Prelude hiding (until, cycle, repeat)
import Control.Arrow qualified as A
import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Boxes.Viewable
import Data.Type.Set
import Data.Type.Flip
import Data.Or
import Data.List.NonEmpty (fromList)
import Data.List.Infinite (Infinite(..), cycle)
import Data.Bool

---------------------------------------------------------------------------

clickOn :: Mouse.Button -> React s (Singleton Mouse.Down) ()
clickOn b = bool (clickOn b) (pure ()) . (== b) =<< Mouse.down

leftClick, middleClick, rightClick :: React s (Singleton Mouse.Down) ()
leftClick = clickOn Mouse.ButtonPrimary
middleClick = clickOn Mouse.ButtonMiddle
rightClick = clickOn Mouse.ButtonSecondary

releaseOn :: Mouse.Button -> React s (Singleton Mouse.Up) ()
releaseOn b = bool (releaseOn b) (pure ()) . (== b) =<< Mouse.up

leftUp, middleUp, rightUp :: React s (Singleton Mouse.Up) ()
leftUp = releaseOn Mouse.ButtonPrimary
middleUp = releaseOn Mouse.ButtonMiddle
rightUp = releaseOn Mouse.ButtonSecondary

before :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') Bool
a `before` b = (<$> a `first` b) \case L _ -> True; _ -> False

doubler :: React s (Mouse.Down :- Singleton TryWait) ()
doubler = do
	adjust rightClick
	r <- rightClick `before` sleep 0.2
	if r then pure () else doubler

---------------------------------------------------------------------------

curRect :: Point -> Sig s (Singleton Mouse.Move) Rect ()
curRect p1 = Rect p1 <$%> Mouse.position

wiggleRect :: Rect -> Sig s (Singleton DeltaTime) Rect ()
wiggleRect (Rect lu rd) = rectAtTime <$%> elapsed where
	rectAtTime t = let dx = sin (realToFrac t * 5) * 15 in
		Rect ((+ dx) `A.first` lu) ((+ dx) `A.first` rd)

firstPoint :: React s (Mouse.Move :- Singleton Mouse.Down) Point
firstPoint = let err = const "never occur" in
	either error id . atResult err err <$> Mouse.position `at` leftClick

completeRect :: Point -> Sig s (Mouse.Move :- Singleton Mouse.Up) Rect Rect
completeRect p1 = (const $ error "never occur") `either` fst
	<$> curRect p1 `until` leftUp

defineRect :: Sig s (Mouse.Move :- Mouse.Down :- Singleton Mouse.Up) Rect Rect
defineRect = adjustSig . completeRect =<< waitFor (adjust firstPoint)

---------------------------------------------------------------------------

colorList :: Infinite BColor
colorList = cycle $ fromList [Red .. Magenta]

cycleColor :: Sig s (Singleton Mouse.Down) BColor ()
cycleColor = go colorList where
	go (h :~ t) = do
		emit h
		bool (pure ()) (go t)
			=<< waitFor (middleClick `before` rightClick)

chooseBoxColor :: Rect -> Sig s (Mouse.Down :- Singleton DeltaTime) Box ()
chooseBoxColor r = Box <$%> adjustSig (wiggleRect r) <*%> adjustSig cycleColor

posInside :: Rect -> Sig s evs Point y -> React s evs ()
posInside rct = void . find (`inside` rct)
	where inside (x, y) (Rect (l, u) (r, d)) =
		(l <= x && x <= r || r <= x && x <= l) &&
		(u <= y && y <= d || d <= y && y <= u)

drClickOn :: Rect -> React s (Mouse.Move :- Mouse.Down :- Singleton TryWait) ()
drClickOn rct = posInside rct
	$ fst <$%> Mouse.position `indexBy` repeat doubler

---------------------------------------------------------------------------

box :: Sig s
	(Mouse.Move :- Mouse.Down :- Mouse.Up :- DeltaTime :- Singleton TryWait)
	Box ()
box = do
	b <- (`Box` Red) <$%> adjustSig defineRect
	adjustSig $ chooseBoxColor b
	waitFor . adjust $ drClickOn b

boxes :: Sig s
	(Mouse.Move :- Mouse.Down :- Mouse.Up :- DeltaTime :- Singleton TryWait)
	[Box] ()
boxes = void $ parList (spawn box)
