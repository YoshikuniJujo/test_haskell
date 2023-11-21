{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Color
import Data.Time
import System.Environment
import System.Exit
import Stopgap.Data.Ptr

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Basic
import Graphics.Cairo.Drawing.CairoT.SaveAndRestore
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.CairoT.Clip
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.ApplicationWindow
	qualified as Gtk.ApplicationWindow
import Stopgap.Graphics.UI.Gtk.DrawingArea qualified as Gtk.DrawingArea
import Stopgap.System.GLib.Application qualified as G.Application
import Stopgap.System.GLib.Signal qualified as G.Signal
import Stopgap.System.GLib.Timeout qualified as G.Timeout

mRadius, mLineWidth :: CDouble
mRadius = 0.42; mLineWidth = 0.05

drawClock :: Gtk.DrawingArea.DrawFunction r Null
drawClock _area cr (fromIntegral -> width) (fromIntegral -> height) Null = do
	cairoScale cr width height
	cairoTranslate cr 0.5 0.5

	cairoSetLineWidth cr mLineWidth
	cairoSave cr

	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0.337 0.612 0.117 0.9
	cairoPaint cr

	cairoRestore cr
	cairoArc cr 0 0 mRadius 0 (2 * pi)
	cairoSave cr

	cairoSetSourceRgba cr . fromJust $ rgbaDouble 1 1 1 0.8
	cairoFillPreserve cr
	cairoRestore cr
	cairoStrokePreserve cr
	cairoClip cr

	for_ [0 :: Int .. 11] \i -> do
		let	inset = 0.05
		cairoSave cr
		cairoSet cr LineCapRound

		inset' <- if (i `mod` 3 /= 0)
		then do	cairoSetLineWidth cr 0.03
			pure $ inset * 0.8
		else pure inset

		cairoMoveTo cr
			((mRadius - inset') * cos (fromIntegral i * pi / 6))
			((mRadius - inset') * sin (fromIntegral i * pi / 6))
		cairoLineTo cr
			(mRadius * cos (fromIntegral i * pi / 6))
			(mRadius * sin (fromIntegral i * pi / 6))

		cairoStroke cr
		cairoRestore cr

	timeinfo <- localTimeOfDay . zonedTimeToLocalTime <$> getZonedTime
	let	hours = fromIntegral (todHour timeinfo) * pi / 6
		minutes = fromIntegral (todMin timeinfo) * pi / 30
		seconds = fromIntegral (round $ todSec timeinfo) * pi / 30

	cairoSave cr
	cairoSet cr LineCapRound
	cairoSave cr

	cairoSet cr . LineWidth $ mLineWidth / 3
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0.7 0.7 0.7 0.8
	cairoMoveTo cr 0 0
	cairoLineTo cr
		(sin seconds * mRadius * 0.9)
		(- cos seconds * mRadius * 0.9)
	cairoStroke cr
	cairoRestore cr

	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0.117 0.337 0.612 0.9
	cairoMoveTo cr 0 0
	cairoLineTo cr
		(sin (minutes + seconds / 60) * mRadius * 0.8)
		(- cos (minutes + seconds / 60) * mRadius * 0.8)
	cairoStroke cr

	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0.337 0.612 0.117 0.9
	cairoMoveTo cr 0 0
	cairoLineTo cr
		(sin (hours + minutes / 12) * mRadius * 0.5)
		(- cos (hours + minutes / 12) * mRadius * 0.5)
	cairoStroke cr
	cairoRestore cr

	cairoArc cr 0 0 (mLineWidth / 3) 0 (2 * pi)
	cairoFill cr

applicationId :: Gtk.Application.Id
applicationId = Gtk.Application.Id "com.github.ToshioCP.da1"

timeHandler :: Gtk.DrawingArea.D -> IO Bool
timeHandler clock = do
	Gtk.Widget.queueDraw clock
	pure True

appActivate :: Gtk.Application.A s -> Null -> IO ()
appActivate app Null = do
	win <- Gtk.ApplicationWindow.new app
	area <- Gtk.DrawingArea.new
	Gtk.Window.setTitle win "da1"
	Gtk.Window.setChild win area

	Gtk.DrawingArea.setDrawFunc area drawClock Null
	void $ G.Timeout.add 1000 timeHandler area
	Gtk.Window.present win

main :: IO ()
main = Gtk.Application.with applicationId G.Application.DefaultFlags \app -> do
	G.Signal.connect app (G.Signal.Signal "activate") appActivate Null
	exitWith =<< join (G.Application.run app <$> getProgName <*> getArgs)
