{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.Color
import System.Environment
import System.Exit
import Stopgap.Data.Ptr

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.ApplicationWindow
	qualified as Gtk.ApplicationWindow
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal

drawFunction :: Gtk.DrawingArea.DrawFunction r Null
drawFunction area cr width height Null = do
	let	squareSize = 40
	cairoSetSourceRgb cr . fromJust $ rgbDouble 1 1 1
	cairoPaint cr
	cairoSetLineWidth cr 2
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	cairoRectangle cr
		(fromIntegral width / 2 - squareSize / 2)
		(fromIntegral height / 2 - squareSize / 2)
		squareSize squareSize
	cairoStroke cr

applicationId :: Gtk.Application.Id
applicationId = Gtk.Application.Id "com.github.ToshioCP.da1"

appActivate :: Gtk.Application.A s -> Null -> IO ()
appActivate app Null = do
	win <- Gtk.ApplicationWindow.new app
	area <- Gtk.DrawingArea.new
	Gtk.Window.setTitle win "da1"
	Gtk.DrawingArea.setDrawFunc area drawFunction Null
	Gtk.Window.setChild win area
	Gtk.Window.present win

main :: IO ()
main = Gtk.Application.with applicationId G.Application.DefaultFlags \app -> do
	G.Signal.connect app (G.Signal.Signal "activate") appActivate Null
	exitWith =<< join (G.Application.run app <$> getProgName <*> getArgs)
