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
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 670 650
	cr <- cairoCreate sr

	cairoMoveTo cr 32 32
	drawLayout cr $ sampleForAlignment LayoutAlignLeft

	cairoMoveTo cr 32 240
	drawLayout cr $ sampleForAlignment LayoutAlignCenter

	cairoMoveTo cr 32 448
	drawLayout cr $ sampleForAlignment LayoutAlignRight

	makePng sr "pngs/try-layout-alignment.png"
