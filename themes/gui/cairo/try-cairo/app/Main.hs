{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Cairo
import Graphics.Cairo.Png

surface :: IO CairoSurfaceT
surface = cairoImageSurfaceCreate cairoFormatArgb32 800 500

main :: IO ()
main = do
	sr <- surface
	cr <- cairoCreate sr

	cairoSetLineWidth cr 2
	cairoSetSourceRgb cr 0.5 0.2 0.2
	cairoRectangle cr 50 30 250 150
	cairoStroke cr

	cairoSetSourceRgb cr 0.2 0.5 0.2
	cairoRectangle cr 350 30 250 150
	cairoFill cr

	print =<< cairoSurfaceWriteToPng sr "tmp.png"
