{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Cairo.Drawing.CairoPatternT.Basic

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 128 128
	cr <- cairoCreate sr
	pt <- cairoPatternCreateLinear 96 32 32 96
	cairoPatternAddColorStopRgb pt 0.2 . fromJust $ rgbDouble 1.0 0.0 0.0
	cairoPatternAddColorStopRgb pt 0.8 . fromJust $ rgbDouble 0.0 0.5 0.5
	print =<< cairoPatternGetColorStopRgbaList pt
	cairoSetSource cr pt
	cairoPaint cr
	pt' <- cairoPatternCreateLinear 32 32 96 96
	cairoPatternAddColorStopRgb pt' 0.2 . fromJust $ rgbDouble 0.5 0.5 0.0
	cairoPatternAddColorStopRgb pt' 0.8 . fromJust $ rgbDouble 0.0 0.0 1.0
	print =<< cairoPatternGetColorStopRgbaList pt'
	cairoSetSource cr pt'
	cairoPaintWithAlpha cr 0.5
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-set-source.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
