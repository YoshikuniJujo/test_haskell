{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Exception
import Data.Foldable
import System.IO
import System.Environment
import System.Exit

import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.ApplicationWindow
	qualified as Gtk.ApplicationWindow
import Stopgap.Graphics.UI.Gtk.Notebook qualified as Gtk.Notebook
import Stopgap.Graphics.UI.Gtk.ScrolledWindow qualified as Gtk.ScrolledWindow
import Stopgap.Graphics.UI.Gtk.TextView qualified as Gtk.TextView
import Stopgap.Graphics.UI.Gtk.TextBuffer qualified as Gtk.TextBuffer
import Stopgap.Graphics.UI.Gtk.Label qualified as Gtk.Label
import Stopgap.System.GLib.Error qualified as G.Error
import Stopgap.System.GLib.Error.Io qualified as G.Error.Io
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Object qualified as G.Object
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.System.GLib.File qualified as G.File
import Stopgap.Data.Ptr

appActivate :: Gtk.Application.A s -> Null -> IO ()
appActivate _app Null = hPutStrLn stderr "You need a filename argument."

appOpen :: (?makeEFuns :: [G.Error.MakeEFun]) =>
	Gtk.Application.A s -> [G.File.F] -> String -> Null -> IO ()
appOpen app files _hint Null = do
	win <- Gtk.ApplicationWindow.new app
	Gtk.Window.setTitle win "file viewer"
	Gtk.Window.setDefaultSize win 400 300
	nb <- Gtk.Notebook.new
	Gtk.Window.setChild win nb

	for_ files \f -> handle (\(e :: G.Error.Io.I) -> print e) do
		(text, _) <- G.File.loadContents f Nothing
		scr <- Gtk.ScrolledWindow.new

		tv <- Gtk.TextView.new
		tb <- Gtk.TextView.getBuffer tv
		Gtk.TextView.setWrapMode tv Gtk.WrapWordChar
		Gtk.TextView.setEditable tv False
		Gtk.ScrolledWindow.setChild scr tv

		Gtk.TextBuffer.setText tb text

		lab <- Gtk.Label.new =<< G.File.getBasename f
		void $ Gtk.Notebook.appendPage nb scr lab
		nbp <- Gtk.Notebook.getPage nb scr
		G.Object.set nbp "tab-expand" True

	nps <- Gtk.Notebook.getNPages nb
	if nps > 0 then Gtk.Window.present win else Gtk.Window.destroy win

main :: IO ()
main = let	?makeEFuns = [G.Error.Io.mkEFun] in
	Gtk.Application.with "com.github.YoshikuniJujo.pr1"
		G.Application.HandlesOpen \app -> do
	G.Signal.connect app "activate" appActivate Null
	G.Signal.connectOpen app "open" appOpen Null
	exitWith =<< join (G.Application.run app <$> getProgName <*> getArgs)
