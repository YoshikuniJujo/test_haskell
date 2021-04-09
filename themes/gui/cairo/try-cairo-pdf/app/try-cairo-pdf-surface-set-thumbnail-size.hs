{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.PdfSurfaces

main :: IO ()
main = cairoPdfSurfaceWith "try-cairo-pdf-surface-set-thumbnail-size.pdf" 595 842 \sr -> do
	cr <- cairoCreate sr
--	cairoPdfSurfaceSetThumbnailSize sr 20 20
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoPaint cr
