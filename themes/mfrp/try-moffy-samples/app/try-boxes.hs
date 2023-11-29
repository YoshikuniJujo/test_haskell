{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (until)

import Control.Arrow qualified as A
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM (atomically, newTChan, readTChan, writeTChan)
import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Handle qualified as H
import Control.Moffy.Handle.TChan
import Control.Moffy.Handle.Time
import Control.Moffy.Run.TChan
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.View qualified as V
import Control.Moffy.Samples.Run.Gtk4
import Data.Type.Set
import Data.Type.Flip
import Data.OneOrMoreApp
import Data.Or
import Data.Bool
import Data.Time
import Data.Time.Clock.System
import Data.Time.Clock.TAI

main :: IO ()
main = do
	er <- atomically newTChan
	eo <- atomically newTChan
	v <- atomically newTChan
	void $ forkIO do
		now <- systemToTAITime <$> getSystemTime
		($ (InitialMode, now)) $ interpretSt
			(H.retrySt . ($ (0.05, ())) . H.popInput . handleTimeEvPlus . H.pushInput . const . H.liftHandle' . H.sleepIfNothing 50000
				$ handleNew @(Mouse.Move :- Mouse.Down :- Singleton DeleteEvent) er eo)
			v do
--			rectToView <$%> curRect (150, 100) `until` deleteEvent
			rectToView <$%> wiggleRect (Rect (150, 100) (300, 200)) `until` deleteEvent
--			waitFor $ doubler `first` deleteEvent
			emit V.Stopped
		putStrLn "AFTER INTERPRET"
	runSingleWin eo v

sameClick :: React s (Singleton Mouse.Down) Bool
sameClick = do
	pressed <- Mouse.down
	pressed2 <- Mouse.down
	pure $ pressed == pressed2

clickOn :: Mouse.Button -> React s (Singleton Mouse.Down) ()
clickOn b0 = do b <- Mouse.down
		bool (clickOn b0) (pure ()) (b == b0)

[leftClick, middleClick, rightClick] = clickOn
	<$> [Mouse.ButtonPrimary, Mouse.ButtonMiddle, Mouse.ButtonSecondary]

before :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') Bool
l `before` r = l `first` r >>= \case L _ -> pure True; _ -> pure False

doubler :: React s (TryWait :- Singleton Mouse.Down) ()
doubler = adjust rightClick
	>> (bool doubler (pure ()) =<< rightClick `before` sleep 0.2)

data BoxesState = BoxesState

curRect :: Point -> Sig s (Singleton Mouse.Move) Rect ()
curRect p1 = Rect p1 <$%> Mouse.position

rectToView :: Rect -> V.View
rectToView (Rect lu rd) = V.Rect lu rd

data Rect = Rect { leftUp :: Point, rightdown :: Point }

type Point = (Double, Double)

wiggleRect :: Rect -> Sig s (Singleton DeltaTime) Rect r
wiggleRect (Rect lu rd) = rectAtTime <$%> elapsed
	where
	rectAtTime t = Rect (A.first (+ dx t) lu) (A.first (+ dx t) rd)
	dx t = sin (realToFrac t * 5) * 15
