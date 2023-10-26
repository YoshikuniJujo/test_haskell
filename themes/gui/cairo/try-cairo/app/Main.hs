{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types

import Data.Maybe
import Data.Color

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Drawing.Text
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Surfaces.PngSupport
import Graphics.Cairo.Types
import Graphics.Cairo.Values

import Control.Monad.ST

import Data.CairoImage.Internal

surface :: IO (CairoSurfaceImageT r RealWorld)
surface = cairoImageSurfaceCreate CairoFormatArgb32 800 500

main :: IO ()
main = do
	sr <- surface
	cr <- cairoCreate sr

	cairoSetLineWidth cr 2
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.2 0.2
	cairoRectangle cr 50 30 250 150
	cairoStroke cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.2 0.5
	cairoRectangle cr 350 30 250 150
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	cairoSelectFontFace cr "Georgia"
		cairoFontSlantNormal cairoFontWeightBold
	cairoSetFontSize cr 48
	te <- cairoTextExtents cr "Hello, world!"
	let	xb = realToFrac $ cairoTextExtentsTXBearing te
		yb = realToFrac $ cairoTextExtentsTYBearing te
		w = realToFrac $ cairoTextExtentsTWidth te
		h = realToFrac $ cairoTextExtentsTHeight te
	cairoMoveTo cr (400 - w / 2 - xb) (250 - h / 2 - yb)
	cairoShowText cr "Hello, world!"

	cairoSetSourceRgb cr .fromJust $ rgbDouble 0 1 0
	cairoPaintWithAlpha cr 0.2

	linpat <- cairoPatternCreateLinear 200 200 600 300
	cairoPatternAddColorStopRgb linpat 0 . fromJust $ rgbDouble 0 0.3 0.8
	cairoPatternAddColorStopRgb linpat 1 . fromJust $ rgbDouble 0 0.8 0.3

	radpat <- cairoPatternCreateRadial 400 250 50 400 250 150
	cairoPatternAddColorStopRgba radpat 0 . fromJust $ rgbaDouble 0 0 0 1
	cairoPatternAddColorStopRgba radpat 0.5 . fromJust $ rgbaDouble 0 0 0 0

	cairoSetSource cr linpat
	cairoMask cr radpat

	print =<< cairoSurfaceWriteToPng (CairoSurfaceTImage sr) "tmp.png"
