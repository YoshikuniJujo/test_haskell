{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Data.ImageData.Text
import Trial.TryPango
import Trial.MakePng

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 900 900
	cr <- cairoCreate sr
	drawLayout cr sampleLayout
	makePng sr "pngs/try-layout.png"
