{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT
import Graphics.Cairo.Surfaces.PdfSurfaces

main :: IO ()
main = do
	sr <- cairoPdfSurfaceCreate "try-pdf.pdf" 128 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoPaint cr
	unsafeCairoSurfaceFinish sr
