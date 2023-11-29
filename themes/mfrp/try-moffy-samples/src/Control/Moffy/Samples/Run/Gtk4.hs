{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Run.Gtk4 where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Moffy
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.View
import Data.Type.Set
import Data.OneOrMoreApp
import Data.Maybe
import Data.Int
import Data.Color
import Stopgap.Data.Ptr
import System.Environment
import System.Exit

import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.ApplicationWindow qualified as
	Gtk.ApplicationWindow
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea
import Stopgap.Graphics.UI.Gtk.EventControllerMotion qualified as
	Gtk.EventControllerMotion
import Stopgap.Graphics.UI.Gtk.GestureClick qualified as Gtk.GestureClick
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.System.GLib.Idle qualified as G.Idle

beforeClose :: TChan (EvOccs (Mouse.Move :- Mouse.Down :- Singleton DeleteEvent)) ->
	Gtk.ApplicationWindow.A -> Null -> IO Bool
beforeClose ceo win Null = do
	putStrLn "BEFORE CLOSE"
	atomically $ writeTChan ceo (expand $ Singleton OccDeleteEvent)
	pure True

appActivate :: TChan (EvOccs (Mouse.Move :- Mouse.Down :- Singleton DeleteEvent)) ->
	TChan View ->
	Gtk.Application.A s -> Null -> IO ()
appActivate ceo cv app Null = do
	crd <- atomically $ newTVar ((50, 30), (200, 150))
	win <- Gtk.ApplicationWindow.new app
	da <- Gtk.DrawingArea.new
	Gtk.DrawingArea.setDrawFunc da (drawFunction crd) Null

	gcp <- mouseButtonHandler ceo Mouse.ButtonPrimary
	gcm <- mouseButtonHandler ceo Mouse.ButtonMiddle
	gcs <- mouseButtonHandler ceo Mouse.ButtonSecondary

	ecm <- Gtk.EventControllerMotion.new
	G.Signal.connectXY ecm (G.Signal.Signal "motion") (moveHandler ceo) Null

	Gtk.Window.setChild win da
	Gtk.Widget.addController da gcp
	Gtk.Widget.addController da gcm
	Gtk.Widget.addController da gcs
	Gtk.Widget.addController da ecm

	forkIO . forever $ atomically (readTChan cv) >>= \case
		Stopped -> void $ G.Idle.add
			(\_ -> Gtk.Window.destroy win >> pure False) Null
		Rect lu rd -> do
			atomically $ writeTVar crd (lu, rd)
			void $ G.Idle.add
				(\_ -> Gtk.Widget.queueDraw da >> pure False)
				Null
		v -> print v

	G.Signal.connectClose win (G.Signal.Signal "close-request") (beforeClose ceo) Null

	Gtk.Window.present win

appId :: Gtk.Application.Id
appId = Gtk.Application.Id "com.github.YoshikuniJujo.moffy-samples-run"

runSingleWin :: TChan (EvOccs (Mouse.Move :- Mouse.Down :- Singleton DeleteEvent)) ->
	TChan View -> IO ()
runSingleWin ceo cv = Gtk.Application.with
		appId G.Application.DefaultFlags \app -> do
	G.Signal.connect app (G.Signal.Signal "activate") (appActivate ceo cv) Null
	exitWith =<< join (G.Application.run app <$> getProgName <*> getArgs)

pressHandler ::
	TChan (EvOccs (Mouse.Move :- Mouse.Down :- Singleton DeleteEvent)) ->
	Mouse.Button ->
	Gtk.GestureClick.G -> Int32 -> Double -> Double -> Null -> IO ()
pressHandler ceo b _gc n x y Null = atomically . writeTChan ceo
	$ expand (Mouse.OccMove (x, y) >- Singleton (Mouse.OccDown b) ::
		EvOccs (Mouse.Move :- Singleton Mouse.Down))

mouseButtonHandler ::
	TChan (EvOccs (Mouse.Move :- Mouse.Down :- Singleton DeleteEvent)) ->
	Mouse.Button ->
	IO Gtk.GestureClick.G
mouseButtonHandler ceo b = do
	gcp <- Gtk.GestureClick.new
	Gtk.GestureClick.setButton gcp (mouseButtonToGesture b)
	G.Signal.connectNXY gcp
		(G.Signal.Signal "pressed") (pressHandler ceo b) Null
	pure gcp

moveHandler ::
	TChan (EvOccs (Mouse.Move :- Mouse.Down :- Singleton DeleteEvent)) ->
	Gtk.EventControllerMotion.E -> Double -> Double -> Null -> IO ()
moveHandler ceo _ x y Null = atomically . writeTChan ceo
	. expand $ Singleton (Mouse.OccMove (x, y))

viewHandler :: TChan view -> Gtk.ApplicationWindow.A -> IO Bool
viewHandler cv win = do
	atomically $ readTChan cv
	putStrLn "VIEW HANDLER"
	pure True

mouseButtonToGesture :: Mouse.Button -> Gtk.GestureClick.Button
mouseButtonToGesture = \case
	Mouse.ButtonPrimary -> Gtk.GestureClick.ButtonPrimary
	Mouse.ButtonMiddle -> Gtk.GestureClick.ButtonMiddle
	Mouse.ButtonSecondary -> Gtk.GestureClick.ButtonSecondary

drawFunction :: TVar (Point, Point) -> Gtk.DrawingArea.DrawFunction r Null
drawFunction crd area cr width height Null = do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 1 1 1
	cairoPaint cr
	cairoSetLineWidth cr 2
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	(	(realToFrac -> l, realToFrac -> u),
		(realToFrac -> r, realToFrac -> d) ) <- atomically $ readTVar crd
	cairoRectangle cr l u (r - l) (d - u)
	cairoFill cr
