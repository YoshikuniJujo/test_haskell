{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import System.Exit

import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.ApplicationWindow
	qualified as Gtk.ApplicationWindow
import Stopgap.Graphics.UI.Gtk.Box qualified as Gtk.Box
import Stopgap.Graphics.UI.Gtk.Button qualified as Gtk.Button
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Data.Ptr

click1Cb :: Gtk.Button.B -> Null -> IO ()
click1Cb b Null = Gtk.Button.getLabel b >>= \case
	"Hello." -> Gtk.Button.setLabel b "Good-bye."
	_ -> Gtk.Button.setLabel b "Hello."

click2Cb :: Gtk.Button.B -> Gtk.ApplicationWindow.A -> IO ()
click2Cb _b w = Gtk.Window.destroy w

appActivate :: Gtk.Application.A s -> Null -> IO ()
appActivate app Null = do
	win <- Gtk.ApplicationWindow.new app
	Gtk.Window.setTitle win "Slozsoft"
	Gtk.Window.setDefaultSize win 400 300

	box <- Gtk.Box.new Gtk.OrientationVertical 5
	Gtk.Box.setHomogeneous box True
	Gtk.Window.setChild win box

	btn1 <- Gtk.Button.newWithLabel "Hello."
	G.Signal.connect btn1 "clicked" click1Cb Null

	btn2 <- Gtk.Button.newWithLabel "Close"
	G.Signal.connect btn2 "clicked" click2Cb win

	Gtk.Box.append box btn1
	Gtk.Box.append box btn2

	Gtk.Window.present win

main :: IO ()
main = Gtk.Application.with
		"com.github.YoshikuniJujo.pr1" G.Application.DefaultFlags \app -> do
	G.Signal.connect app "activate" appActivate Null
	exitWith =<< G.Application.run app =<< getArgs
