{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (until)

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
import Control.Moffy.Samples.View
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
			curRect (150, 100) `until` deleteEvent
--			waitFor $ doubler `first` deleteEvent
			emit Stopped
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

curRect :: Point -> Sig s (Singleton Mouse.Move) View ()
curRect p1 = Rect p1 <$%> Mouse.position
