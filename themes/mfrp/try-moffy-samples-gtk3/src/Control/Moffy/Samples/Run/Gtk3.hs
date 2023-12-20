{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Run.Gtk3 where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Type.Set
import Data.OneOrMoreApp
import System.Environment

import Control.Moffy
import Control.Moffy.Samples.Event.Delete
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Event.CalcTextExtents
import Control.Moffy.Samples.View

import Stopgap.Data.Ptr
import Stopgap.System.GLib qualified as G
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Container qualified as Gtk.Container
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea
import Stopgap.Graphics.UI.Gdk.Event qualified as Gdk.Event
import Stopgap.Graphics.UI.Gdk.Event.Button qualified as Gdk.Event.Button

type Events = CalcTextExtents :-
	Mouse.Move :- Mouse.Down :- Mouse.Up :- Singleton DeleteEvent

clicked :: TChan (EvOccs Events) ->
	Gtk.DrawingArea.D -> Gdk.Event.Button.B -> ud -> IO Bool
clicked ceo _da eb _ud = do
	case Gdk.Event.Button.bType eb of
		Gdk.Event.ButtonPress -> atomically . writeTChan ceo
			$ expand (Singleton . Mouse.OccDown $ mouseButton eb)
		_ -> pure ()
	pure True

mouseButton :: Gdk.Event.Button.B -> Mouse.Button
mouseButton eb = case Gdk.Event.Button.bButton eb of
	1 -> Mouse.ButtonPrimary
	2 -> Mouse.ButtonMiddle
	3 -> Mouse.ButtonSecondary
	_ -> Mouse.ButtonMiddle

runSingleWin ::
	TChan (EvReqs Events) -> TChan (EvOccs Events) -> TChan View -> IO ()
runSingleWin cer ceo cv = do
	join $ Gtk.init <$> getProgName <*> getArgs

	w <- Gtk.Window.new Gtk.Window.Toplevel
	G.Signal.connect_void_void w "destroy" Gtk.mainQuit Null

	da <- Gtk.DrawingArea.new
	Gtk.Container.add w da
	Gtk.Widget.addEvents da Gdk.Event.ButtonPressMask
	G.Signal.connect_self_button_ud
		da "button-press-event" (clicked ceo) Null

	Gtk.Widget.showAll w

	forkIO . forever $ atomically (readTChan cv) >>= \case
		Stopped -> void $ G.idleAdd
			(\_ -> Gtk.mainQuit >> pure False) Null
		v -> print v

	Gtk.main
