{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT.Basic
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = readImage "data/HaskellLogo.png" >>= \case
	Right (ImageRGBA8 i) -> do
		let i' = juicyRGBA8ToCairoArgb32 i
		sr <- cairoImageSurfaceCreate CairoFormatArgb32 256 256
		cr <- cairoCreate sr
		cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
		sr' <- cairoImageSurfaceCreateForCairoImage $ CairoImageArgb32 i'
		cairoMaskSurface cr sr' 64 64

		print $ cairoSurfaceGetType sr
		print $ cairoSurfaceGetType sr'
		pure (cairoSurfaceGetType sr) >>= \case
			CairoSurfaceTypeImage -> putStrLn "CairoSurfaceTypeImage"
			_ -> putStrLn "other type"
		print $ cairoSurfaceGetContent sr
		print $ cairoSurfaceGetContent sr'
		pure (cairoSurfaceGetContent sr) >>= \case
			CairoContentColorAlpha -> putStrLn "CairoSurfaceColorAlpha"
			_ -> putStrLn "other content"

		cairoImageSurfaceGetCairoImage sr >>= \case
			CairoImageArgb32 ci ->
				writePng "try-cairo-mask-surface.png" $ cairoArgb32ToJuicyRGBA8 ci
			_ -> error "never occur"
	_ -> error "bad"
