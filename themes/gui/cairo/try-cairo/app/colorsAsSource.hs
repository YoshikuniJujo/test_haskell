{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Cairo
import Graphics.Cairo.Paths
import Graphics.Cairo.Png

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 500 500
	cr <- cairoCreate sr

	cairoSetSourceRgb cr 0 0 0
	cairoMoveTo cr 0 0
	cairoLineTo cr 500 500
	cairoMoveTo cr 500 0
	cairoLineTo cr 0 500
	cairoSetLineWidth cr 100
	cairoStroke cr

	cairoRectangle cr 0 0 250 250
	cairoSetSourceRgba cr 1 0 0 0.8
	cairoFill cr

	cairoRectangle cr 0 250 250 250
	cairoSetSourceRgba cr 0 1 0 0.6
	cairoFill cr

	cairoRectangle cr 250 0 250 250
	cairoSetSourceRgba cr 0 0 1 0.4
	cairoFill cr

	print =<< cairoSurfaceWriteToPng sr "colorsAsSource.png"
