{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.Int
import Data.Color
import Data.CairoImage.Internal
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Surfaces.PngSupport

width, height, squareSize :: Int32
width = 100; height = 100; squareSize = 40

main :: IO ()
main = do
	surface <- cairoImageSurfaceCreate CairoFormatRgb24 width height
	cr <- cairoCreate surface

	cairoSetSourceRgb cr . fromJust $ rgbDouble 1 1 1
	cairoPaint cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	cairoSetLineWidth cr 2
	cairoRectangle cr
		(fromIntegral width / 2 - fromIntegral squareSize / 2)
		(fromIntegral height / 2 - fromIntegral squareSize / 2)
		(fromIntegral squareSize) (fromIntegral squareSize)
	cairoStroke cr

	void $ cairoSurfaceWriteToPng surface "result/rectangle.png"
