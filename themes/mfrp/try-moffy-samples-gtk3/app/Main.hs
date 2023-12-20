{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.ST
import Data.CairoContext
import System.Environment
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Stopgap.Data.Ptr
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Container qualified as Gtk.Container
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea
import Stopgap.Graphics.UI.Gdk.Event qualified as Gdk.Event
import Stopgap.Graphics.UI.Gdk.Event.Button qualified as Gdk.Event.Button

clicked :: Gtk.DrawingArea.D -> Gdk.Event.Button.B -> ud -> IO Bool
clicked _da eb _ud = do
	print eb
	pure True

drawCallback :: Gtk.DrawingArea.D -> CairoT r RealWorld -> Null -> IO Bool
drawCallback _ cr Null = do
	cairoMoveTo cr 150 100
	cairoLineTo cr 300 200
	cairoStroke cr
	pure False

cbDelete :: Gtk.Window.W -> ud -> IO Bool
cbDelete w ud = pure False

main :: IO ()
main = do
	join $ Gtk.init <$> getProgName <*> getArgs

	w <- Gtk.Window.new Gtk.Window.Toplevel
	G.Signal.connect_ab_bool w "delete-event" cbDelete Null
	G.Signal.connect_void_void w "destroy" Gtk.mainQuit Null

	da <- Gtk.DrawingArea.new
	Gtk.Container.add w da
	G.Signal.connect_self_cairo_ud da "draw" drawCallback Null
	Gtk.Widget.addEvents da Gdk.Event.ButtonPressMask
	G.Signal.connect_self_button_ud da "button-press-event" clicked Null

	Gtk.Widget.showAll w
	Gtk.main
