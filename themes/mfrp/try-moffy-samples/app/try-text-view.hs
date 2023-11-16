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
import Stopgap.Graphics.UI.Gtk.ScrolledWindow qualified as Gtk.ScrolledWindow
import Stopgap.Graphics.UI.Gtk.TextView qualified as Gtk.TextView
import Stopgap.Graphics.UI.Gtk.TextBuffer qualified as Gtk.TextBuffer
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Data.Ptr

appActivate :: Gtk.Application.A s -> Null -> IO ()
appActivate app Null = do
	let	text =	"Once upon a time, there was an old man " ++
			"who was called Taketori-no-Okina. " ++
			"It is a japanese word that means a man " ++
			"whose work is making bamboo baskets.\n" ++
			"One day, he went into a hill and found " ++
			"a shining bamboo. " ++
			"\"What a mysterious bamboo it is!, \" he said. " ++
			"He cut it, then there was " ++
			"a small cute baby girl in it. " ++
			"The girl was shining faintly. " ++
			"He thought this baby girl is a gift from Heaven " ++
			"and took her home.\n" ++
			"His wife was surprized at his story. " ++
			"Tey were very happy because they had no children. "
	win <- Gtk.ApplicationWindow.new app
	Gtk.Window.setTitle win "Slozsoft"
	Gtk.Window.setDefaultSize win 400 300

	scr <- Gtk.ScrolledWindow.new
	Gtk.Window.setChild win scr

	tv <- Gtk.TextView.new
	tb <- Gtk.TextView.getBuffer tv
	Gtk.TextBuffer.setText tb text
	Gtk.TextView.setWrapMode tv Gtk.WrapWordChar

	Gtk.ScrolledWindow.setChild scr tv

	Gtk.Window.present win

main :: IO ()
main = Gtk.Application.with
		"com.github.YoshikuniJujo.pr1" G.Application.DefaultFlags \app -> do
	G.Signal.connect app "activate" appActivate Null
	exitWith =<< G.Application.run app =<< getArgs
