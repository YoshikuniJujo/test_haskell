{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
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
import Control.Moffy.Samples.Boxes.Viewable
import Data.Type.Set
import Data.Type.Flip
import Data.Maybe
import Data.Time.Clock.System
import Data.Color

runBoxes :: forall s es r . (
	Adjustable
		(Merge es (Singleton DeleteEvent))
		(CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- DeltaTime :- TryWait :- DeleteEvent :- 'Nil),
	Firstable es (Singleton DeleteEvent) (ISig s (es :+: Singleton DeleteEvent) V.View r) () ) =>
	Sig s es [Box] r -> IO ()
runBoxes bxs = runBoxes_ $ boxesToView <$%> (emit [] >> bxs)

runBoxes_ :: forall s es r . (
	Adjustable
		(Merge es (Singleton DeleteEvent))
		(CalcTextExtents :- Mouse.Move :- Mouse.Down :- Mouse.Up :- DeltaTime :- TryWait :- DeleteEvent :- 'Nil),
	Firstable es (Singleton DeleteEvent) (ISig s (es :+: Singleton DeleteEvent) V.View r) () ) =>
	Sig s es V.View r -> IO ()
runBoxes_ b = do
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

boxesToView :: [Box] -> V.View
boxesToView = V.View . (boxToView1 <$>)

boxToView1 :: Box -> V.View1
boxToView1 (Box (Rect lu rd) c) = V.Box lu rd $ bColorToColor c

bColorToColor :: BColor -> Rgb Double
bColorToColor = fromJust . \case
	Red -> rgbDouble 0.8 0.1 0.05
	Green -> rgbDouble 0.2 0.6 0.1
	Blue -> rgbDouble 0.2 0.2 0.8
	Yellow -> rgbDouble 0.8 0.7 0.1
	Cyan -> rgbDouble 0.2 0.6 0.6
	Magenta -> rgbDouble 0.5 0.2 0.4
