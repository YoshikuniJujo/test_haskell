{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Data.Foldable
import Data.Maybe
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
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 500 600
	cr <- cairoCreate sr

	for_ (zip [0 .. 20] (cycle [0, 1])) \(y, d) ->
		for_ (zip [0 .. 10] (drop d $ cycle [snow, grey])) \(x, sg) -> do
			cairoSetSourceRgb cr sg
			cairoRectangle cr (x * 50) (y * 50) 50 50
			cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.2 0.1
	cairoMoveTo cr 16 16
	drawLayout cr sampleForColor

	makePng sr "pngs/try-color.png"

snow, grey :: Rgb CDouble
snow = fromJust $ rgbDouble 0.7 0.7 0.7
grey = fromJust $ rgbDouble 0.6 0.6 0.6
