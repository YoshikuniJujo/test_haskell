{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

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

	cairoSetLineWidth cr 5

	cairoMoveTo cr 125 125
	cairoLineTo cr 250 187.5
	cairoRelLineTo cr 125 (- 62.5)
	cairoArc cr 250 250 (125 * sqrt 2) (- 0.25 * pi) (0.25 * pi)
	cairoRelCurveTo cr (- 125) (- 62.5) (- 125) 62.5 (- 250) 0
	cairoClosePath cr

	cairoStroke cr

	print =<< cairoSurfaceWriteToPng (CairoSurfaceTImage sr) "tryPath.png"
