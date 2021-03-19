{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.CairoSurfaceT
import Graphics.Cairo.Surfaces.SvgSurfaces

main :: IO ()
main = do
	sr <- cairoSvgSurfaceCreate "try-svg.svg" 128 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoPaint cr
--	cairoSurfaceFlush sr
	cairoSurfaceFinish sr
