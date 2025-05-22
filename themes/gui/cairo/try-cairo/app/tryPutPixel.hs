{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage.Internal
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Surfaces.ImageSurfaces

import Parts (checkPattern, writeArgb32Mut)

main :: IO ()
main = do
	img <- newImageMut @Argb32Mut 16 16
	let	img' = CairoImageMutArgb32 img

	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 16 16
	cr <- cairoCreate sfc0

	putMultiPixels img [
		(3, 3), (3, 4), (3, 5), (3, 6),
		(4, 3), (4, 4), (4, 5), (4, 6),
		(5, 3), (5, 4), (5, 5), (5, 6),
		(6, 3), (6, 4), (6, 5), (6, 6)
		] $ PixelArgb32Straight 255 255 255 0

	putMultiPixels img [
		(2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7),
		(3, 2),                                 (3, 7),
		(4, 2),                                 (4, 7),
		(5, 2),                                 (5, 7),
		(6, 2),                                 (6, 7),
		(7, 2), (7, 3), (7, 4), (7, 5), (7, 6), (7, 7)
		] $ PixelArgb32Straight 255 0 0 0

	sfc <- CairoSurfaceTImage <$> cairoImageSurfaceCreateForCairoImageMut img'
	ptn <- cairoPatternCreateForSurface sfc
	cairoSetSource cr ptn
	cairoPaint cr

	cairoImageSurfaceGetCairoImageMut sfc0 >>= \case
		CairoImageMutArgb32 i -> writeArgb32Mut "tryPutPixel.png" i
		_ -> error "image format error"

putMultiPixels _ [] _ = pure ()
putMultiPixels img ((x, y) : xys) c = putPixel img x y c >> putMultiPixels img xys c
