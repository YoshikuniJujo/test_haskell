{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Foldable

import Graphics.Cairo
import Graphics.Cairo.CairoT
import Graphics.Cairo.Pattern
import Graphics.Cairo.Png

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 500 500
	cr <- cairoCreate sr

	radpat <- cairoPatternCreateRadial 125 125 50 250 250 250
	cairoPatternAddColorStopRgb radpat 0 1 0.8 0.8
	cairoPatternAddColorStopRgb radpat 1 0.9 0 0

	for_ [1 .. 9] \i -> for_ [1 .. 9] \j ->
		cairoRectangle cr (50 * i - 20) (50 * j - 20) 40 40
	cairoSetSource cr radpat
	cairoFill cr

	linpat <- cairoPatternCreateLinear 125 175 375 325
	cairoPatternAddColorStopRgba linpat 0.00 1 1 1 0
	cairoPatternAddColorStopRgba linpat 0.25 0 1 0 0.5
	cairoPatternAddColorStopRgba linpat 0.50 1 1 1 0
	cairoPatternAddColorStopRgba linpat 0.75 0 0 1 0.5
	cairoPatternAddColorStopRgba linpat 1.00 1 1 1 0

	cairoRectangle cr 0 0 500 500
	cairoSetSource cr linpat
	cairoFill cr

	print =<< cairoSurfaceWriteToPng sr "gradientsAsSource.png"
