{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Run.Gtk4 where

import Control.Monad
import Control.Concurrent.STM
import Control.Moffy
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.Delete
import Data.Type.Set
import Data.OneOrMoreApp
import Data.Int
import Stopgap.Data.Ptr
import System.Environment
import System.Exit

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.ApplicationWindow qualified as
	Gtk.ApplicationWindow
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea
import Stopgap.Graphics.UI.Gtk.GestureClick qualified as Gtk.GestureClick
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal

beforeClose :: TChan (EvOccs (Mouse.Down :- Singleton DeleteEvent)) ->
	Gtk.ApplicationWindow.A -> Null -> IO Bool
beforeClose ceo win Null = do
	putStrLn "BEFORE CLOSE"
	atomically $ writeTChan ceo (expand $ Singleton OccDeleteEvent)
	pure False

appActivate :: TChan (EvOccs (Mouse.Down :- Singleton DeleteEvent)) ->
	Gtk.Application.A s -> Null -> IO ()
appActivate ceo app Null = do
	win <- Gtk.ApplicationWindow.new app
	da <- Gtk.DrawingArea.new
	gcp <- Gtk.GestureClick.new
	Gtk.GestureClick.setButton gcp Gtk.GestureClick.ButtonPrimary

	Gtk.Window.setChild win da
	Gtk.Widget.addController da gcp

	G.Signal.connectNXY gcp
		(G.Signal.Signal "pressed") (pressHandler ceo) Null
	G.Signal.connectClose win (G.Signal.Signal "close-request") (beforeClose ceo) Null

	Gtk.Window.present win

appId :: Gtk.Application.Id
appId = Gtk.Application.Id "com.github.YoshikuniJujo.moffy-samples-run"

runSingleWin :: TChan (EvOccs (Mouse.Down :- Singleton DeleteEvent)) -> IO ()
runSingleWin ceo = Gtk.Application.with
		appId G.Application.DefaultFlags \app -> do
	G.Signal.connect app (G.Signal.Signal "activate") (appActivate ceo) Null
	exitWith =<< join (G.Application.run app <$> getProgName <*> getArgs)

pressHandler :: TChan (EvOccs (Mouse.Down :- Singleton DeleteEvent)) ->
	Gtk.GestureClick.G -> Int32 -> Double -> Double -> Null -> IO ()
pressHandler ceo _gc n x y Null = do
	atomically $ writeTChan ceo . expand . Singleton $ Mouse.OccDown Mouse.ButtonPrimary
