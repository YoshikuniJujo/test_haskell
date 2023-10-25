{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Color
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
	drawLayout cr $ sampleForUnderline [
		UnderlineNone,
		Underline UnderlineSingle Nothing,
		Underline UnderlineSingle $ rgbDouble 0.2 0.7 0.1,
		Underline UnderlineDouble Nothing,
		Underline UnderlineDouble $ rgbDouble 0.2 0.7 0.1,
		Underline UnderlineLow Nothing,
		Underline UnderlineLow $ rgbDouble 0.2 0.7 0.1,
		Underline UnderlineError Nothing,
		Underline UnderlineError $ rgbDouble 0.2 0.7 0.1
		]

	makePng sr "pngs/try-underline.png"
