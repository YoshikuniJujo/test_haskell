{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import System.Exit

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.ApplicationWindow
	qualified as Gtk.ApplicationWindow
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Data.Ptr

appActivate :: Gtk.Application.A -> Null -> IO ()
appActivate app Null = do
	win <- Gtk.ApplicationWindow.new app
	Gtk.Window.setTitle win "Slozsoft"
	Gtk.Window.setDefaultSize win 400 300
	Gtk.Window.present win

main :: IO ()
main = do
	app <- Gtk.Application.new
		"com.github.YoshikuniJujo.pr1" G.Application.DefaultFlags
	G.Signal.connect app "activate" appActivate Null
	exitWith =<< G.Application.run app =<< getArgs
