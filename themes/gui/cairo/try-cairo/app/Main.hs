{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Cairo
import Graphics.Cairo.Png

surface :: IO CairoSurfaceT
surface = cairoImageSurfaceCreate cairoFormatArgb32 500 300

main :: IO ()
main = do
	sr <- surface
	cr <- cairoCreate sr
	cairoSetLineWidth cr 2
	cairoRectangle cr 50 30 250 150
	cairoStroke cr
	print =<< cairoSurfaceWriteToPng sr "tmp.png"
