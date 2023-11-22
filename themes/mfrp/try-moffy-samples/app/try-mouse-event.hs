{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment
import System.Exit
import Stopgap.Data.Ptr
import Data.Int

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.ApplicationWindow qualified as
	Gtk.ApplicationWindow
import Stopgap.Graphics.UI.Gtk.EventControllerMotion qualified as
	Gtk.EventControllerMotion
import Stopgap.Graphics.UI.Gtk.GestureClick qualified as Gtk.GestureClick
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
	gcp <- Gtk.GestureClick.new
	Gtk.GestureClick.setButton gcp Gtk.GestureClick.ButtonPrimary
	gcm <- Gtk.GestureClick.new
	Gtk.GestureClick.setButton gcm Gtk.GestureClick.ButtonMiddle
	gcs <- Gtk.GestureClick.new
	Gtk.GestureClick.setButton gcs Gtk.GestureClick.ButtonSecondary

	Gtk.Window.setChild win da
	Gtk.Widget.addController da em
	Gtk.Widget.addController da gcp
	Gtk.Widget.addController da gcm
	Gtk.Widget.addController da gcs

	Gtk.DrawingArea.setDrawFunc da drawFunction Null
	G.Signal.connect em (G.Signal.Signal "leave") leaveHandler Null
	G.Signal.connectXY em
		(G.Signal.Signal "motion") (moveEnterHandler "motion") Null
	G.Signal.connectXY em
		(G.Signal.Signal "enter") (moveEnterHandler "enter") Null
	G.Signal.connectNXY gcp
		(G.Signal.Signal "pressed") (pressHandler "primary") Null
	G.Signal.connectNXY gcm
		(G.Signal.Signal "pressed") (pressHandler "middle") Null
	G.Signal.connectNXY gcs
		(G.Signal.Signal "pressed") (pressHandler "secondary") Null

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

pressHandler :: String -> Gtk.GestureClick.G -> Int32 -> Double -> Double -> Null -> IO ()
pressHandler b _gc n x y Null = do
	putStrLn $ b ++ " pressed: n = " ++ show n ++
		" x = " ++ show x ++ " y = " ++ show y
