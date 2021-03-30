{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import CairoMeshPattern

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 512 512
	cr <- cairoCreate sr

	pt <- cairoPatternCreateMesh
	cairoMeshPatternBeginPatch pt
	cairoMeshPatternMoveTo pt 64 448
	cairoMeshPatternLineTo pt 64 64
	cairoMeshPatternLineTo pt 448 64
	cairoMeshPatternLineTo pt 448 448
	cairoMeshPatternSetCornerColorRgb pt 0 1 0 0
	cairoMeshPatternSetCornerColorRgb pt 1 0 1 0
	cairoMeshPatternSetCornerColorRgb pt 2 0 0 1
	cairoMeshPatternSetCornerColorRgb pt 3 1 1 0
	cairoMeshPatternEndPatch pt

	cairoSetSource cr pt
	cairoPaint cr

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "mesh.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
