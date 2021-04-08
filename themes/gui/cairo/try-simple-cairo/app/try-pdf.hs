{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Page
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.TagsAndLinks
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT
import Graphics.Cairo.Surfaces.PdfSurfaces

main :: IO ()
main = cairoPdfSurfaceWith "try-pdf.pdf" 595 842 \sr -> do
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoPaint cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.2 0.1
	cairoRectangle cr 50 50 50 50
	cairoTagLinkInternal cr "here" $ cairoFill cr
	cairoRectangle cr 400 50 50 50
	cairoFill cr
--	cairoTagLinkInternal cr (Right (2, (50, 100))) $ cairoFill cr
	cairoCopyPage cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.2 0.1
	cairoRectangle cr 198.33 280.67 198.33 280.67
	cairoTagLinkUri cr "https://google.com" $ cairoFill cr
	cairoShowPage cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.2 0.5
	cairoRectangle cr 198.33 280.67 198.33 280.67
	cairoFill cr
	cairoRectangle cr 400 600 50 50
	cairoTagDestination cr "here" $ cairoFill cr
