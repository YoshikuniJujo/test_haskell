{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Data.ImageData.Text
import Trial.TryPango
import Trial.MakePng

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 670 650
	cr <- cairoCreate sr

	cairoMoveTo cr 32 32
	drawLayout cr sampleForShape

	cairoMoveTo cr 32 256
	drawLayout cr sampleForScale

	makePng sr "pngs/try-shape.png"
