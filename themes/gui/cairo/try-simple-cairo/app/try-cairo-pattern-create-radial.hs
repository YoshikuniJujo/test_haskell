{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT.Basic
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 128 128
	cr <- cairoCreate sr
	pt <- cairoPatternCreateRadial 16 64 4 64 64 64
	cairoPatternAddColorStopRgb pt 0.2 . fromJust $ rgbDouble 0.7 0.3 0.2
	cairoPatternAddColorStopRgb pt 0.5 . fromJust $ rgbDouble 0.52 0.52 0.2
	cairoPatternAddColorStopRgb pt 0.8 . fromJust $ rgbDouble 0.3 0.7 0.2
	print =<< cairoPatternGetRadialCircles pt
	print =<< cairoPatternGetColorStopRgbaList pt
	cairoSetSource cr pt
	cairoPaint cr
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-pattern-create-radial.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
