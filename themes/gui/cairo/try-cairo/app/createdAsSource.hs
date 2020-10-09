{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Cairo
import Graphics.Cairo.CairoT
import Graphics.Cairo.Path
import Graphics.Cairo.Png

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 500 500
	cr <- cairoCreate sr

	cairoSetSourceRgb cr 0 0 1
	cairoMoveTo cr 100 100
	cairoLineTo cr 400 400
	cairoStroke cr

	cairoPushGroup cr
	cairoSetSourceRgb cr 1 0 0
	cairoRectangle cr 50 150 400 200
	cairoFill cr

	cairoPushGroup cr
	cairoSetSourceRgb cr 0 1 0
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

	print =<< cairoSurfaceWriteToPng sr "createdAsSource.png"
