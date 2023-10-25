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
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 900 900
	cr <- cairoCreate sr
	drawLayout cr sampleLayout

	cairoMoveTo cr 32 200
	drawLayout cr $ sampleForWidth LayoutWidthDefault

	cairoMoveTo cr 32 280
	drawLayout cr . sampleForWidth $ LayoutWidth 500

	cairoMoveTo cr 32 570
	drawLayout cr . sampleForWidth $ LayoutWidth 700

	makePng sr "pngs/try-layout.png"
