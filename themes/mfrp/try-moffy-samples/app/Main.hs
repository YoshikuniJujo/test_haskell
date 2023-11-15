{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import System.Exit

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.Data.Ptr

appActivate :: Gtk.Application.A -> Null -> IO ()
appActivate _app Null = putStrLn "GtkApplication is activated."

main :: IO ()
main = do
	app <- Gtk.Application.new
		"com.github.YoshikuniJujo.pr1" G.Application.DefaultFlags
	G.Signal.connect app "activate" appActivate Null
	exitWith
		=<< G.Application.run (Gtk.Application.gApplication app)
		=<< getArgs
