{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Surfaces.PngSupport
import Graphics.Cairo.Values

import Data.CairoImage.Internal

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 500 500
	cr <- cairoCreate sr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	cairoMoveTo cr 0 0
	cairoLineTo cr 500 500
	cairoMoveTo cr 500 0
	cairoLineTo cr 0 500
	cairoSetLineWidth cr 100
	cairoStroke cr

	cairoRectangle cr 0 0 250 250
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 1 0 0 0.8
	cairoFill cr

	cairoRectangle cr 0 250 250 250
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0 1 0 0.6
	cairoFill cr

	cairoRectangle cr 250 0 250 250
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0 0 1 0.4
	cairoFill cr

	print =<< cairoSurfaceWriteToPng (CairoSurfaceTImage sr) "colorsAsSource.png"
