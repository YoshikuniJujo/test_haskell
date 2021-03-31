{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Cairo.Drawing.CairoPatternT.Mesh

import Data.Maybe
import Data.Color

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 512 512
	cr <- cairoCreate sr

	pt <- cairoPatternCreateMesh

{-
	cairoMeshPatternBeginPatch pt
	cairoMeshPatternMoveTo pt 64 128
	cairoMeshPatternLineTo pt 64 64
	cairoMeshPatternLineTo pt 128 64
	cairoMeshPatternLineTo pt 128 128
	cairoMeshPatternSetCornerColorRgb pt 0 1 0 0
	cairoMeshPatternSetCornerColorRgb pt 1 0 1 0
	cairoMeshPatternSetCornerColorRgb pt 2 0 0 1
	cairoMeshPatternSetCornerColorRgb pt 3 1 1 0
	cairoMeshPatternEndPatch pt

	cairoMeshPatternBeginPatch pt
	cairoMeshPatternMoveTo pt 256 128
	cairoMeshPatternLineTo pt 256 64
	cairoMeshPatternLineTo pt 320 64
	cairoMeshPatternSetCornerColorRgb pt 0 0 1 1
	cairoMeshPatternSetCornerColorRgb pt 1 1 0 1
	cairoMeshPatternSetCornerColorRgb pt 2 1 1 0
	cairoMeshPatternEndPatch pt
	-}

	cairoMeshPatternAddPatch pt
		(MoveTo 128 128) (LineTo 128 64) (LineTo 192 64) (LineTo 192 128) CloseLineTo
		(ColorRgb . fromJust $ rgbDouble 1 1 0)
		(ColorRgb . fromJust $ rgbDouble 0 0 1)
		(ColorRgb . fromJust $ rgbDouble 0 1 0)
		(ColorRgb . fromJust $ rgbDouble 1 0 0)
		Nothing Nothing Nothing Nothing

	cairoMeshPatternAddPatch pt
		(MoveTo 256 128)
		(CurveTo 224 106.6 224 85.3 256 64)
		(LineTo 320 64)
		(LineTo 320 128)
		CloseLineTo
		(ColorRgb . fromJust $ rgbDouble 0 1 1)
		(ColorRgb . fromJust $ rgbDouble 1 0 1)
		(ColorRgb . fromJust $ rgbDouble 1 1 0)
		(ColorRgb . fromJust $ rgbDouble 1 1 1)
		Nothing Nothing Nothing Nothing

	cairoSetSource cr pt
	cairoPaint cr

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "mesh.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
