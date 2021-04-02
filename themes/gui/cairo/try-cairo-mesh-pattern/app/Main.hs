{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths.CairoPathT (pattern CairoPathT, pattern CairoPathTPatch)
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Cairo.Drawing.CairoPatternT.Mesh

import Data.Maybe
import Data.Color

import Foreign.C.Types
import Control.Monad.Primitive
import Data.CairoContext

main :: IO ()
main = do
--	sr <- cairoImageSurfaceCreate cairoFormatArgb32 512 512
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 512 336
	cr <- cairoCreate sr

	pt <- cairoPatternCreateMesh

	cairoMeshPatternAddPatchDefaultControlPoints pt
		(MoveTo 128 128) (LineTo 128 64) (LineTo 192 64) (LineTo 192 128) CloseLineTo
		(ColorRgb . fromJust $ rgbDouble 1 1 0)
		(ColorRgb . fromJust $ rgbDouble 0 0 1)
		(ColorRgb . fromJust $ rgbDouble 0 1 0)
		(ColorRgb . fromJust $ rgbDouble 1 0 0)

	cairoMeshPatternAddPatch pt
		(MoveTo 128 256) (LineTo 128 192) (LineTo 192 192) (LineTo 192 256) CloseLineTo
		(ColorRgb . fromJust $ rgbDouble 1 1 0)
		(ColorRgb . fromJust $ rgbDouble 0 0 1)
		(ColorRgb . fromJust $ rgbDouble 0 1 0)
		(ColorRgb . fromJust $ rgbDouble 1 0 0)
		(Just $ Point 128 256) (Just $ Point 128 192) (Just $ Point 32 192) (Just $ Point 32 256)

	cairoMeshPatternAddPatchDefaultControlPoints pt
		(MoveTo 256 128) (CurveTo 224 106.6 224 85.3 256 64)
		(LineTo 320 64) (LineTo 320 128)
		CloseLineTo
		(ColorRgb . fromJust $ rgbDouble 0 1 1)
		(ColorRgb . fromJust $ rgbDouble 1 0 1)
		(ColorRgb . fromJust $ rgbDouble 1 1 0)
		(ColorRgb . fromJust $ rgbDouble 1 1 1)

	cairoMeshPatternAddPatch pt
		(MoveTo 256 256) (CurveTo 224 234.6 224 213.3 256 192)
		(LineTo 320 192) (LineTo 320 256)
		CloseLineTo
		(ColorRgb . fromJust $ rgbDouble 0 1 1)
		(ColorRgb . fromJust $ rgbDouble 1 0 1)
		(ColorRgb . fromJust $ rgbDouble 1 1 0)
		(ColorRgb . fromJust $ rgbDouble 1 1 1)
		(Just $ Point 277.33 234.67) (Just $ Point 277.33 213.33)
		(Just $ Point 298.64 213.33) (Just $ Point 298.64 234.67)

	cairoMeshPatternAddPatchDefaultControlPoints3 pt
		(MoveTo 384 128) (LineTo 448 128) (LineTo 416 72.57) CloseLineTo
		(ColorRgb . fromJust $ rgbDouble 0 0 1)
		(ColorRgb . fromJust $ rgbDouble 0 1 0)
		(ColorRgb . fromJust $ rgbDouble 1 0 0)

	ps <- cairoMeshPatternGetPatchList pt
	print ps
	cairoSetSource cr pt
	cairoPaint cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	strokeControlPoints cr pt

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "mesh.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"


	let	(ppth, _, _) = head ps
		ptht@(CairoPathT pth) = CairoPathTPatch ppth
	putStrLn ""
	print pth
	print ptht
	
	let	CairoPathTPatch ppth' = ptht
	print ppth'

strokeControlPoints :: PrimBase m =>
	CairoT (PrimState m) -> CairoPatternMeshT (PrimState m) -> m ()
strokeControlPoints cr pm = do
	mapM_ (drawLines cr . getPointsFromMesh) =<< cairoMeshPatternGetPatchList pm
	cairoStroke cr

getPointsFromMesh :: (a, b, (Point, Point, Point, Point)) -> [(CDouble, CDouble)]
getPointsFromMesh (_, _, (Point x0 y0, Point x1 y1, Point x2 y2, Point x3 y3)) =
	[(x0, y0), (x1, y1), (x2, y2), (x3, y3)]

drawLines :: PrimMonad m =>
	CairoT (PrimState m) -> [(CDouble, CDouble)] -> m ()
drawLines _ [] = pure ()
drawLines cr ((x0, y0) : xys) = cairoMoveTo cr x0 y0 >> uncurry (cairoLineTo cr) `mapM_` xys
