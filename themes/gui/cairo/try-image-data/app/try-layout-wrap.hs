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
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 900 1200
	cr <- cairoCreate sr

	cairoMoveTo cr 32 32
	drawLayout cr $ sampleForWrap (LayoutWidth 600) LayoutWrapWord

	cairoMoveTo cr 32 200
	drawLayout cr $ sampleForWrap (LayoutWidth 600) LayoutWrapChar

	cairoMoveTo cr 32 370
	drawLayout cr $ sampleForWrap (LayoutWidth 600) LayoutWrapWordChar

	cairoMoveTo cr 650 16
	drawLayout cr $ sampleForWrap (LayoutWidth 80) LayoutWrapWord

	cairoMoveTo cr 760 16
	drawLayout cr $ sampleForWrap (LayoutWidth 80) LayoutWrapWordChar

	makePng sr "pngs/try-layout-wrap.png"
