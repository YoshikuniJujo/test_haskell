{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Data.ImageData
import Trial.TryCairo
import Trial.MakePng

import Graphics.Cairo.Drawing.CairoT

main :: IO ()
main = pngWith "pngs/simple.png" 128 128 \cr -> do
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0 0 0 0
	cairoPaint cr
	cairoDrawSurface cr Surface {
		sfcTrans = Transform 1 0 0 1 0 0,
		sfcSource = Source
			. PatternColor . fromJust $ rgbaDouble 0.4 0.8 0.2 1.0,
		sfcMask = MaskPaint 0.5 }
