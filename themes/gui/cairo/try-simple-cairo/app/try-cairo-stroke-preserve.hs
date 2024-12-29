{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 128 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoRectangle cr 32 32 64 64
	cairoStrokePreserve cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.8 0.1
	cairoRectangle cr 32 32 64 64
	cairoFill cr
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-stroke-preserve.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
