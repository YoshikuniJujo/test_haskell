{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import MakePng

main :: IO ()
main = pngWith "pngs/try-transform.png" 768 896 \cr -> do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.4 0.05
	cairoPaint cr
	cairoSet cr $ LineWidth 16
	cairoMoveTo cr 16 16
	cairoTranslate cr 512 64
	cairoLineTo cr 128 128
	cairoTranslate cr (- 512) 64
	cairoLineTo cr 128 128
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.05 0.2 0.025
	cairoStroke cr
