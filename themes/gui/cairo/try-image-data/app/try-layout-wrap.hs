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
	drawLayout cr $ sampleForWrap (LayoutWidth 600) LayoutWrapWord LayoutEllipsizeNone

	cairoMoveTo cr 32 200
	drawLayout cr $ sampleForWrap (LayoutWidth 600) LayoutWrapChar LayoutEllipsizeNone

	cairoMoveTo cr 32 370
	drawLayout cr $ sampleForWrap (LayoutWidth 600) LayoutWrapWordChar LayoutEllipsizeNone

	cairoMoveTo cr 650 16
	drawLayout cr $ sampleForWrap (LayoutWidth 80) LayoutWrapWord LayoutEllipsizeNone

	cairoMoveTo cr 760 16
	drawLayout cr $ sampleForWrap (LayoutWidth 80) LayoutWrapWordChar LayoutEllipsizeNone

	makePng sr "pngs/try-layout-wrap.png"
