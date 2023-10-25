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
	drawLayout cr . sampleForLineSpacing $ LayoutLineSpacing 0

	cairoMoveTo cr 32 232
	drawLayout cr . sampleForLineSpacing $ LayoutLineSpacing 1

	cairoMoveTo cr 32 432
	drawLayout cr . sampleForLineSpacing $ LayoutLineSpacing 0.8

	cairoMoveTo cr 32 632
	drawLayout cr . sampleForLineSpacing $ LayoutLineSpacing 1.5

	cairoMoveTo cr 32 882
	drawLayout cr . sampleForLineSpacing $ LayoutLineSpacing 2

	makePng sr "pngs/try-layout-line-spacing.png"
