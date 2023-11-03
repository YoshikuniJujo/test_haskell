{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SampleImages where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Maybe
import Data.Color
import Data.CairoImage.Internal
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

import Graphics.Cairo.Surfaces.PngSupport
import Graphics.Cairo.Drawing.CairoPatternT

import Data.ByteString qualified as BS

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
