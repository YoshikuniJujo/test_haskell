{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.ForeignPtr
import Data.CairoContext
import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoPatternT.Basic

import CairoMeshPattern

import Graphics.Cairo.Exception

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 512 512
	cr <- cairoCreate sr

	pt <- cairoPatternCreateMesh

	cairoMeshPatternBeginPatch pt
{-
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

	cairoMeshPatternBeginPatch pt
	cairoMeshPatternMoveTo pt 128 128
	cairoMeshPatternLineTo pt 128 64
	cairoMeshPatternLineTo pt 192 64
	cairoMeshPatternLineTo pt 192 128
	cairoMeshPatternSetCornerColorRgb pt 0 0 0 0
	cairoMeshPatternSetCornerColorRgb pt 1 0 0 1
	cairoMeshPatternSetCornerColorRgb pt 2 0 1 0
	cairoMeshPatternSetCornerColorRgb pt 3 1 0 0
	cairoMeshPatternEndPatch pt
	-}

	cairoSetSource cr pt
	putStrLn "BEFORE PAINT"
	cairoPaint cr

	putStrLn "BEFORE CHECK CAIRO T ERROR"
	let CairoT fcr = cr in
		cairoStatusToThrowError =<< withForeignPtr fcr c_cairo_status
	putStrLn "BEFORE CHECK CAIRO PATTERN T ERROR"
	let CairoPatternT fpt = pt in
		cairoStatusToThrowError =<< withForeignPtr fpt c_cairo_pattern_status

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "mesh.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
