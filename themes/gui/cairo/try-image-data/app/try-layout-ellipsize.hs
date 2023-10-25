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

	cairoMoveTo cr 32 190
	drawLayout cr $ sampleForWrap (LayoutWidth 600) LayoutWrapWord LayoutEllipsizeStart

	cairoMoveTo cr 32 260
	drawLayout cr $ sampleForWrap (LayoutWidth 600) LayoutWrapWord LayoutEllipsizeMiddle

	cairoMoveTo cr 32 330
	drawLayout cr $ sampleForWrap (LayoutWidth 600) LayoutWrapWord LayoutEllipsizeEnd

	cairoMoveTo cr 32 450
	drawLayout cr $ sampleForHeight (LayoutWidth 600) LayoutEllipsizeEnd LayoutHeightDefault

	cairoMoveTo cr 32 520
	drawLayout cr . sampleForHeight (LayoutWidth 600) LayoutEllipsizeEnd $ LayoutHeight 100

	cairoMoveTo cr 32 640
	drawLayout cr . sampleForHeight (LayoutWidth 600) LayoutEllipsizeEnd $ LayoutLines 2

	cairoMoveTo cr 32 760
	drawLayout cr . sampleForJustify $ LayoutJustify False

	cairoMoveTo cr 32 900
	drawLayout cr . sampleForJustify $ LayoutJustify True

	makePng sr "pngs/try-layout-ellipsize.png"
