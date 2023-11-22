{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment
import System.Exit
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.ApplicationWindow qualified as
	Gtk.ApplicationWindow
import Stopgap.Graphics.UI.Gtk.EventControllerMotion qualified as
	Gtk.EventControllerMotion
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal

main :: IO ()
main = Gtk.Application.with
		(Gtk.Application.Id "com.github.YoshikuniJujo.try-mouse-event")
		G.Application.DefaultFlags \app -> do
	G.Signal.connect app (G.Signal.Signal "activate") appActivate Null
	exitWith =<< join (G.Application.run app <$> getProgName <*> getArgs)

appActivate :: Gtk.Application.A s -> Null -> IO ()
appActivate app Null = do
	win <- Gtk.ApplicationWindow.new app
	da <- Gtk.DrawingArea.new
	em <- Gtk.EventControllerMotion.new

	Gtk.Window.setChild win da
	Gtk.Widget.addController da em

	Gtk.DrawingArea.setDrawFunc da drawFunction Null
	G.Signal.connect em (G.Signal.Signal "leave") leaveHandler Null
	G.Signal.connectXY em
		(G.Signal.Signal "motion") (moveEnterHandler "motion") Null
	G.Signal.connectXY em
		(G.Signal.Signal "enter") (moveEnterHandler "enter") Null

	Gtk.Window.present win

drawFunction :: Gtk.DrawingArea.DrawFunction r Null
drawFunction _area _cr _width _height Null = do
	pure ()

moveEnterHandler ::
	String -> Gtk.EventControllerMotion.E -> Double -> Double -> Null ->
	IO ()
moveEnterHandler nm _em x y Null =
	putStrLn $ nm ++ ": " ++ "x = " ++ show x ++ " y = " ++ show y

leaveHandler :: Gtk.EventControllerMotion.E -> Null -> IO ()
leaveHandler _em Null = putStrLn "leave"
