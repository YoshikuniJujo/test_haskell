{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Boxes.Run (runBoxes) where

import Prelude hiding (until)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Handle qualified as H
import Control.Moffy.Handle.Time
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.Event.CalcTextExtents
import Control.Moffy.Samples.Handle.TChan
import Control.Moffy.Samples.View qualified as V
import Control.Moffy.Samples.Run.TChan
import Control.Moffy.Samples.Run.Gtk4
import Data.Type.Set
import Data.Time.Clock.System

runBoxes :: forall s es r . (
	Adjustable
		(Merge es (Singleton DeleteEvent))
		(CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- DeltaTime :- TryWait :- DeleteEvent :- 'Nil),
	Firstable es (Singleton DeleteEvent) (ISig s (es :+: Singleton DeleteEvent) V.View r) () ) =>
	Sig s es V.View r -> IO ()
runBoxes b = do
	er <- atomically newTChan
	eo <- atomically newTChan
	v <- atomically newTChan
	void $ forkIO do
		now <- systemToTAITime <$> getSystemTime
		void . ($ (InitialMode, now)) $ interpretSt
			(H.retrySt . ($ (0.05, ())) . H.popInput . handleTimeEvPlus . H.pushInput . const . H.liftHandle' . H.sleepIfNothing 50000
				$ handleNew @(CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent) er eo)
			v do
			_ <- b `until` deleteEvent :: Sig s (Merge es (Singleton DeleteEvent)) V.View (Either r (V.View, ()))
			emit V.Stopped
	runSingleWin er eo v
