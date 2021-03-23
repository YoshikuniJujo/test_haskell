{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Cairo.Drawing.CairoPatternT

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 128 128
	cr <- cairoCreate sr
	pt <- cairoPatternCreateLinear 96 32 32 96
	cairoPatternAddColorStopRgb pt 0.2 1.0 0.0 0.0
	cairoPatternAddColorStopRgb pt 0.8 0.0 0.5 0.5
	cairoSetSource cr pt
	cairoPaint cr
	pt' <- cairoPatternCreateLinear 32 32 96 96
	cairoPatternAddColorStopRgb pt' 0.2 0.5 0.5 0.0
	cairoPatternAddColorStopRgb pt' 0.8 0.0 0.0 1.0
	cairoSetSource cr pt'
	cairoPaintWithAlpha cr 0.5
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-set-source.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
