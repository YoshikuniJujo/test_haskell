{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoPatternT

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 128 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	pt <- cairoPatternCreateLinear 96 32 32 96
	cairoPatternAddColorStopRgba pt 0.2 0 0 0 0
	cairoPatternAddColorStopRgba pt 0.8 0 0 0 1
	cairoMask cr pt
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-mask.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
