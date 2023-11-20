{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Exception
import System.IO
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
import Stopgap.System.GLib.Error qualified as G.Error
import Stopgap.System.GLib.Error.Io qualified as G.Error.Io
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.System.GLib.File qualified as G.File
import Stopgap.Data.Ptr

appActivate :: Gtk.Application.A s -> Null -> IO ()
appActivate _app Null = hPutStrLn stderr "You need a filename argument."

appOpen :: (?makeEFuns :: [G.Error.MakeEFun]) =>
	Gtk.Application.A s -> [G.File.F] -> String -> Null -> IO ()
appOpen app files _hint Null = do
	win <- Gtk.ApplicationWindow.new app
	Gtk.Window.setDefaultSize win 400 300
	scr <- Gtk.ScrolledWindow.new
	Gtk.Window.setChild win scr

	tv <- Gtk.TextView.new
	tb <- Gtk.TextView.getBuffer tv
	Gtk.TextView.setWrapMode tv Gtk.WrapWordChar
	Gtk.TextView.setEditable tv False
	Gtk.ScrolledWindow.setChild scr tv

	(text, _) <- G.File.loadContents (files !! 0) Nothing
		`onException` Gtk.Window.destroy win
	Gtk.TextBuffer.setText tb text
	Gtk.Window.setTitle win "Slozsoft"

	Gtk.Window.present win

main :: IO ()
main = let	?makeEFuns = [G.Error.Io.mkEFun] in
	Gtk.Application.with
		"com.github.YoshikuniJujo.pr1" G.Application.HandlesOpen \app -> do
	G.Signal.connect app "activate" appActivate Null
	G.Signal.connectOpen app "open" appOpen Null
	cmd <- getProgName
	exitWith =<< G.Application.run app cmd =<< getArgs
