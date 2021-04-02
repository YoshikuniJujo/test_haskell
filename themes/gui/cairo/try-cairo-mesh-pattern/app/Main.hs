{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths.CairoPathT (pattern CairoPathT)
import Graphics.Cairo.Drawing.Paths
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

	cairoMeshPatternAddPatch pt
		(MoveTo 128 128) (LineTo 128 64) (LineTo 192 64) (LineTo 192 128) CloseLineTo
		(ColorRgb . fromJust $ rgbDouble 1 1 0)
		(ColorRgb . fromJust $ rgbDouble 0 0 1)
		(ColorRgb . fromJust $ rgbDouble 0 1 0)
		(ColorRgb . fromJust $ rgbDouble 1 0 0)
		Nothing Nothing Nothing Nothing

	cairoMeshPatternAddPatch pt
		(MoveTo 256 128) (CurveTo 224 106.6 224 85.3 256 64)
		(LineTo 320 64) (LineTo 320 128)
		CloseLineTo
		(ColorRgb . fromJust $ rgbDouble 0 1 1)
		(ColorRgb . fromJust $ rgbDouble 1 0 1)
		(ColorRgb . fromJust $ rgbDouble 1 1 0)
		(ColorRgb . fromJust $ rgbDouble 1 1 1)
		Nothing Nothing Nothing Nothing

	cairoMeshPatternAddPatch3 pt
		(MoveTo 384 128) (LineTo 448 128) (LineTo 416 72.57) CloseLineTo
		(ColorRgb . fromJust $ rgbDouble 0 0 1)
		(ColorRgb . fromJust $ rgbDouble 0 1 0)
		(ColorRgb . fromJust $ rgbDouble 1 0 0)
		Nothing Nothing Nothing

	ps <- cairoMeshPatternGetPatchList pt
	print ps
	let	[
			(_, _, (
				Point a0 b0, Point a1 b1,
				Point a2 b2, Point a3 b3 )),
			(_, _, (
				Point c0 d0, Point c1 d1,
				Point c2 d2, Point c3 d3 )),
			(CairoPathT cp, _, (
				Point x0 y0, Point x1 y1,
				Point x2 y2, Point x3 y3 )) ] = ps
	print cp
	cairoSetSource cr pt
	cairoPaint cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0

	cairoMoveTo cr a0 b0
	uncurry (cairoLineTo cr) `mapM_` [(a1, b1), (a2, b2), (a3, b3)]

	cairoMoveTo cr c0 d0
	uncurry (cairoLineTo cr) `mapM_` [(c1, d1), (c2, d2), (c3, d3)]

	cairoMoveTo cr x0 y0
	cairoLineTo cr x1 y1
	cairoLineTo cr x2 y2
	cairoLineTo cr x3 y3
	cairoStroke cr

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "mesh.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
