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
	cr <- cairoCreate $ CairoSurfaceTImage sr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 1
	cairoMoveTo cr 100 100
	cairoLineTo cr 400 400
	cairoStroke cr

	cairoPushGroup cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 1 0 0
	cairoRectangle cr 50 150 400 200
	cairoFill cr

	cairoPushGroup cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 1 0
	cairoRectangle cr 150 50 200 400
	cairoFill cr

	p1 <- cairoPopGroup cr
	p2 <- cairoPopGroup cr
	cairoSetSource cr p1
	cairoPaint cr
	cairoSetSource cr p2
	cairoPaint cr

{-
	cairoPopGroupToSource cr
	cairoPaint cr
	cairoPopGroupToSource cr
	cairoPaint cr
	-}

	print =<< cairoSurfaceWriteToPng (CairoSurfaceTImage sr) "createdAsSource.png"
