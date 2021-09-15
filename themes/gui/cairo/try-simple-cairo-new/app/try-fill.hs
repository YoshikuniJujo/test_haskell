{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths
import MakePng

main :: IO ()
main = pngWith "pngs/try-fill.png" 768 896 \cr -> do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.3 0.05
	cairoPaint cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.05 0.1 0.05
	cairoSet cr $ LineWidth 32
	cairoSet cr LineJoinRound
	cairoRectangle cr 64 64 128 128
	cairoFill cr
