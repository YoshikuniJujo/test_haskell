{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SampleImages where

import Foreign.C
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Maybe
import Data.Int
import Data.Color
import Data.CairoImage.Internal
import Data.CairoContext
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Graphics.Cairo.Surfaces.PngSupport
import Graphics.Cairo.Drawing.CairoPatternT

import Data.ByteString qualified as BS

simpleGlaph :: (Real n, Real n') => Int32 -> Int32 -> n -> n -> n' -> n' -> [n] -> Argb32
simpleGlaph w h
	(realToFrac -> x0) (realToFrac -> y0) (realToFrac -> dx) (realToFrac -> dy) ((realToFrac <$>) -> vs) =
	case runST put of
		CairoImageArgb32 i -> i
		_ -> error "never occur"
	where
	put :: ST s CairoImage
	put = do
		sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 w h
		cr <- cairoCreate sfc0
		putSimpleGlaph cr x0 dx (((+ y0) . (* dy)) <$> vs)
		cairoImageSurfaceGetCairoImage sfc0

putSimpleGlaph :: PrimMonad m =>
	CairoT r (PrimState m) -> CDouble -> CDouble -> [CDouble] -> m ()
putSimpleGlaph cr x0 dx (v : vs) = do
	cairoMoveTo cr x0 v
	go (x0 + dx) vs
	where
	go _ [] = cairoStroke cr
	go x (v : vs) = do
		cairoLineTo cr x v
		go (x + dx) vs

twoRectangles :: Argb32
twoRectangles = case runST twoRectanglesPrim of
	CairoImageArgb32 i -> i
	_ -> error "never occur"

twoRectanglesPrim :: PrimMonad m => m CairoImage
twoRectanglesPrim = do
	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 256 256
	cr <- cairoCreate sfc0

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.7
	cairoRectangle cr 0 0 256 256
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.3
	cairoRectangle cr 50 50 110 110
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.3
	cairoRectangle cr 100 130 100 70
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.5
	cairoMoveTo cr 70 70
	cairoLineTo cr 200 200
	cairoStroke cr

	putSimpleGlaph cr 40 20 [50, 70, 30, 90, 100, 20]

	cairoImageSurfaceGetCairoImage sfc0

fromPng :: IO Argb32
fromPng = do
	sfc <- cairoImageSurfaceCreate CairoFormatArgb32 512 512
	cr <- cairoCreate sfc

	sfc' <- cairoSurfaceCreateFromPng "../../../../files/images/saikoro.png"
	cairoSetSourceSurface cr sfc' 0 0
	cairoPaint cr

	cairoImageSurfaceGetCairoImage sfc >>= \case
		CairoImageArgb32 i -> pure i
		_ -> error "never occur"

fromPng' :: IO Argb32
fromPng' = do
	sfc <- cairoImageSurfaceCreate CairoFormatArgb32 768 512
	cr <- cairoCreate sfc

	bs <- BS.readFile "../../../../files/images/saikoro.png"
	sfc' <- cairoSurfaceCreateFromPngByteString bs
	cairoSetSourceSurface cr sfc' 0 0
	cairoPaint cr

	cairoImageSurfaceGetCairoImage sfc >>= \case
		CairoImageArgb32 i -> pure i
		_ -> error "never occur"
