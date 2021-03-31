{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths.Basic
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Cairo.Drawing.CairoPatternT.Basic

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 128 128
	cr <- cairoCreate sr
	pts <- cairoPatternCreateRgb . fromJust $ rgbDouble 0.2 0.8 0.1
	let	pt = CairoPatternTSolid pts
		ptt = cairoPatternGetType pt
	print ptt
	print CairoPatternTypeSolid
	case ptt of
		CairoPatternTypeSolid -> putStrLn "CairoPatternTypeSolid"
		_ -> putStrLn "other"
	print =<< cairoPatternGetRgba pts
	cairoSetSource cr pts
	cairoRectangle cr 32 32 64 64
	cairoFill cr
	pts' <- cairoPatternCreateRgba . fromJust $ rgbaDouble 0.0 0.0 0.8 0.4
	cairoSetSource cr pts'
	cairoPaint cr
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-pattern-create-rgb.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
