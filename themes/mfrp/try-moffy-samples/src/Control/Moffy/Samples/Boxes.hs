{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Boxes where

import Prelude hiding (until, repeat, cycle)

import Control.Arrow qualified as A
import Control.Monad
import Control.Monad.Trans.Except (pattern ExceptT, runExceptT)
import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Boxes.Viewable
import Data.Type.Set
import Data.Type.Flip
import Data.Bool
import Data.Or
import Data.List.NonEmpty (fromList)
import Data.List.Infinite (Infinite(..), cycle)

sameClick :: React s (Singleton Mouse.Down) Bool
sameClick = do
	pressed <- Mouse.down
	pressed2 <- Mouse.down
	pure $ pressed == pressed2

clickOn :: Mouse.Button -> React s (Singleton Mouse.Down) ()
clickOn b0 = do
	b <- Mouse.down
	bool (clickOn b0) (pure ()) (b == b0)

leftClick, middleClick, rightClick :: React s (Singleton Mouse.Down) ()
leftClick = clickOn Mouse.ButtonPrimary
middleClick = clickOn Mouse.ButtonMiddle
rightClick = clickOn Mouse.ButtonSecondary

releaseOn :: Mouse.Button -> React s (Singleton Mouse.Up) ()
releaseOn b0 = do
	b <- Mouse.up
	bool (releaseOn b0) (pure ()) (b == b0)

leftUp :: React s (Singleton Mouse.Up) ()
leftUp = releaseOn Mouse.ButtonPrimary

before :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') Bool
l `before` r = (\case L _ -> True; _ -> False) <$> l `first` r

doubler :: React s (TryWait :- Singleton Mouse.Down) ()
doubler = adjust rightClick >>
	rightClick `before` sleep 0.2 >>= bool doubler (pure ())

curRect :: Point -> Sig s (Singleton Mouse.Move) Rect ()
curRect p1 = Rect p1 <$%> Mouse.position

wiggleRect :: Rect -> Sig s (Singleton DeltaTime) Rect r
wiggleRect (Rect lu rd) = rectAtTime <$%> elapsed
	where rectAtTime t = let dx = sin (realToFrac t * 5) * 15 in
		Rect ((+ dx) `A.first` lu) ((+ dx) `A.first` rd)

posInside :: Rect -> Sig s es Point r -> React s es (Either Point r)
posInside rct = find (`inside` rct)
	where (x, y) `inside` Rect (l, u) (r, d) =
		(l <= x && x <= r || r <= x && x <= l) &&
		(u <= y && y <= d || d <= y && y <= u)

firstPoint :: React s (Mouse.Down :- Mouse.Move :- 'Nil) (Either String Point)
firstPoint = atResult (const "firstPoint 1") (const "firstPoint 2")
	<$> Mouse.position `at` leftClick

completeRect :: Point ->
	Sig s (Mouse.Up :- Mouse.Move :- 'Nil) Rect (Either String Rect)
completeRect p1 = const (Left "never occur") `either` (Right . fst)
	<$> curRect p1 `until` leftUp

defineRect :: Sig s Mouse.Events Rect Rect
defineRect = either error pure =<< runExceptT do
	p1 <- ExceptT . waitFor $ adjust firstPoint
	ExceptT . adjustSig $ completeRect p1

cycleColor :: Sig s (Mouse.Down :- 'Nil) BColor ()
cycleColor = go . cycle $ fromList [Red .. Magenta] where
	go (h :~ t) = do
		emit h
		(bool (pure ()) (go t)
			=<< waitFor (middleClick `before` rightClick))

chooseBoxColor :: Rect -> Sig s (Mouse.Down :- DeltaTime :- 'Nil) Box ()
chooseBoxColor r = Box <$%> adjustSig (wiggleRect r) <*%> adjustSig cycleColor

drClickOn :: Rect -> React s (Mouse.Down :- Mouse.Move :- TryWait :- 'Nil) ()
drClickOn rct = void . posInside rct
	. (fst <$%>) $ Mouse.position `indexBy` repeat doubler

box :: Sig s (Mouse.Events :+: DeltaTime :- TryWait :- 'Nil) Box ()
box = (`Box` Red) <$%> adjustSig defineRect >>= \b -> do
	adjustSig $ chooseBoxColor b
	waitFor . adjust $ drClickOn b

boxes :: Sig s (Mouse.Events :+: DeltaTime :- TryWait :- 'Nil) [Box] ()
boxes = () <$ parList (spawn box)
