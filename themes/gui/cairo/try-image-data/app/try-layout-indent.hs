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
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 900 1200
	cr <- cairoCreate sr

	cairoMoveTo cr 32 32
	drawLayout cr . sampleForIndent $ LayoutIndent 0

	cairoMoveTo cr 32 232
	drawLayout cr . sampleForIndent $ LayoutIndent 10

	cairoMoveTo cr 32 432
	drawLayout cr . sampleForIndent $ LayoutIndent 100

	cairoMoveTo cr 32 632
	drawLayout cr . sampleForIndent $ LayoutIndent (- 10)

	cairoMoveTo cr 32 832
	drawLayout cr . sampleForIndent $ LayoutIndent (- 100)

	makePng sr "pngs/try-layout-indent.png"
