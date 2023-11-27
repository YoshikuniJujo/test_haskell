{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM (atomically, newTChan, writeTChan)
import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Handle.Time
import Control.Moffy.Run.TChan
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.View
import Control.Moffy.Samples.Run.Gtk4
import Data.Type.Set
import Data.OneOrMoreApp
import Data.Or
import Data.Bool

import Trial.Boxes

main :: IO ()
main = do
	er <- atomically newTChan
	eo <- atomically newTChan
	v <- atomically newTChan
	void $ forkIO do
		($ ()) $ interpretSt
--			(retrySt . ($ ()) . popInput . handleTimeEvPlus . pushInput . const . liftHandle' . sleepIfNothing 100000
			(retrySt . ($ ()) . popInput . pushInput . const . liftHandle' . sleepIfNothing 100000
				$ handleNew @(Mouse.Down :- Singleton DeleteEvent) er eo)
			v do
			waitFor $ clickOn Mouse.ButtonPrimary `first` deleteEvent
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

data BoxesState = BoxesState
