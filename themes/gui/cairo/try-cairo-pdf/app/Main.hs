{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.PdfSurfaces

main :: IO ()
main = cairoPdfSurfaceWith "try-cairo-pdf.pdf" 595 842 \sr -> do
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoPaint cr
