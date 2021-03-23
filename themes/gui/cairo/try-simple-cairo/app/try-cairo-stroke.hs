{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT.Basic
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 128 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoRectangle cr 32 32 64 64
	print =<< cairoStrokeExtents cr
	CairoExtentsLeftTopWidthHeight {
		cairoExtentsLeft = l, cairoExtentsTop = t, cairoExtentsWidth = w, cairoExtentsHeight = h } <- cairoStrokeExtents cr
	print (l, t, w, h)
	cairoStroke cr
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-stroke.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
