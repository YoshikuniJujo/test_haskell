{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import System.Exit

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.ApplicationWindow
	qualified as Gtk.ApplicationWindow
import Stopgap.Graphics.UI.Gtk.TextView qualified as Gtk.TextView
import Stopgap.Graphics.UI.Gtk.TextBuffer qualified as Gtk.TextBuffer
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Data.Ptr

appActivate :: Gtk.Application.A -> Null -> IO ()
appActivate app Null = do
	let	text =	"Once upon a time, " ++
			"there was an old man who was called Taketori-no-Okina. "

	win <- Gtk.ApplicationWindow.new app
	Gtk.Window.setTitle win "Slozsoft"
	Gtk.Window.setDefaultSize win 400 300

	tv <- Gtk.TextView.new
	tb <- Gtk.TextView.getBuffer tv
	Gtk.TextBuffer.setText tb text
	Gtk.Window.setChild win tv

	Gtk.Window.present win

main :: IO ()
main = do
	app <- Gtk.Application.new
		"com.github.YoshikuniJujo.pr1" G.Application.DefaultFlags
	G.Signal.connect app "activate" appActivate Null
	exitWith =<< G.Application.run app =<< getArgs
