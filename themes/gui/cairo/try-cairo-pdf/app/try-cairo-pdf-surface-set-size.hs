{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Page
import Graphics.Cairo.Surfaces.PdfSurfaces

main :: IO ()
main = cairoPdfSurfaceWith "try-cairo-pdf-surface-set-size.pdf" 595 842 \sr -> do
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoPaint cr
	cairoShowPage cr

	cairoPdfSurfaceSetSize sr 128 64
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.4 0.2 0.1
	cairoPaint cr
	cairoShowPage cr

	cairoPdfSurfaceSetSize sr 1000 32
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.2
	cairoPaint cr
	cairoShowPage cr

	cairoPdfSurfaceSetSize sr 32 1000
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.2 0.4
	cairoPaint cr
