{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent
import Data.Foldable
import Data.Maybe
import Data.Color

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.Windows
import Graphics.Gdk.GdkDrawingContext

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Regions
import Graphics.Cairo.Utilities.Types

main :: IO ()
main = do
	d <- gdkDisplayOpen ""
	wr <- gdkScreenGetRootWindow $ gdkDisplayGetDefaultScreen d
	w <- gdkWindowGetWidth wr
	h <- gdkWindowGetHeight wr
	r <- cairoRegionCreateRectangle $ CairoRectangleIntT 0 0 w h
	gdkWindowShow wr
	for_ [0 .. 400] \clk -> do
		gdkWindowWithDrawFrame wr r \cxt -> do
			cr <- gdkDrawingContextGetCairoContext cxt
			cairoSetSourceRgba cr
				. fromJust $ rgbaDouble 0.5 0.5 0.5 1.0
			cairoRectangle cr (clk * 2) 100 500 500
			cairoFill cr
		gdkDisplayFlush d
		threadDelay 12500
