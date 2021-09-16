{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Angle
import Data.Color
import Graphics.Cairo.Drawing.CairoT.Basic
import Graphics.Cairo.Drawing.Paths.Basic
import MakePng

main :: IO ()
main = pngWith "pngs/try-arc.png" 768 896 \cr -> do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.4 0.05
	cairoPaint cr
	cairoArc cr 128 128 32 (Degree 0) (Degree 360)
	cairoNewSubPath cr
	cairoArc cr 256 128 64 (Degree 0) (Degree 120)
	cairoNewSubPath cr
	cairoArcNegative cr 512 128 64 (Degree 0) (Degree 120)
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.05 0.1 0.025
	cairoFill cr
