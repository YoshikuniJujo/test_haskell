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
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 1000 1200
	cr <- cairoCreate sr

	cairoMoveTo cr 32 32
	drawLayout cr $ sampleForAutoDir
		(LayoutWidth 700) (LayoutAutoDir True)

	cairoMoveTo cr 32 128
	drawLayout cr $ sampleForAutoDir
		(LayoutWidth 700) (LayoutAutoDir False)

	cairoMoveTo cr 32 256
	drawLayout cr . sampleForSingleParagraph $ LayoutSingleParagraph False

	cairoMoveTo cr 32 416
	drawLayout cr . sampleForSingleParagraph $ LayoutSingleParagraph True

	makePng sr "pngs/try-layout-auto-dir.png"
