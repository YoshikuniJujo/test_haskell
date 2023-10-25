{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Data.ImageData.Text
import Trial.TryPango
import Trial.MakePng

import Data.CairoImage.Internal

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 670 850
	cr <- cairoCreate sr

	cairoMoveTo cr 32 32
	drawLayout cr sampleForShape

	cairoMoveTo cr 32 256
	drawLayout cr sampleForScale

	cairoMoveTo cr 32 576
	drawLayout cr sampleForRise

	makePng sr "pngs/try-shape.png"
