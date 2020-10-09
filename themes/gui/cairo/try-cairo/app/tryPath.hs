{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Cairo
import Graphics.Cairo.Path
import Graphics.Cairo.Png

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 500 500
	cr <- cairoCreate sr

	cairoSetLineWidth cr 5

	cairoMoveTo cr 125 125
	cairoLineTo cr 250 187.5
	cairoRelLineTo cr 125 (- 62.5)
	cairoArc cr 250 250 (125 * sqrt 2) (- 0.25 * pi) (0.25 * pi)

	cairoStroke cr

	print =<< cairoSurfaceWriteToPng sr "tryPath.png"
