{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.ST
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Surfaces.ImageSurfaces

import Data.Time

main :: IO ()
main = do
	img <- newImageMut @Argb32Mut 16 16
	writeDrawPipe "foobar.png" 16 16 example img

writeDrawPipe :: FilePath -> Int -> Int -> (Argb32Mut RealWorld -> IO ()) -> Argb32Mut RealWorld -> IO ()
writeDrawPipe fp wdt hgt act img = do
	let	img' = CairoImageMutArgb32 img

	act img

	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 (fromIntegral wdt) (fromIntegral hgt)
	cr <- cairoCreate sfc0

	sfc <- CairoSurfaceTImage <$> cairoImageSurfaceCreateForCairoImageMut img'
	ptn <- cairoPatternCreateForSurface sfc
	cairoSetSource cr ptn
	cairoPaint cr

	cairoImageSurfaceGetCairoImageMut sfc0 >>= \case
		CairoImageMutArgb32 i -> writeArgb32Mut fp i
		_ -> error "bad"

putPixels img [] c = pure ()
putPixels img ((x, y) : xys) c = putPixel img x y c >> putPixels img xys c

writeArgb32Mut :: FilePath -> Argb32Mut RealWorld -> IO ()
writeArgb32Mut fp = (writePng fp =<<) . cairoArgb32MutToJuicyRGBA8

example img = do
	i <- (`mod` 5) . floor . utctDayTime <$> getCurrentTime
	print . utctDayTime =<< getCurrentTime
	j <- floor . (* 50) . utctDayTime <$> getCurrentTime
	print i
	print j
	putPixels img
		[ (x, y) | x <- [0 .. 15], y <- [0 .. 15] ]
		(PixelArgb32Straight 255 0 0 0)
	putPixels img
		[ (x, y) | x <- [1 .. 14], y <- [1 .. 14] ]
		(PixelArgb32Straight 255 128 128 128)
	putPixels img
		[ (x, y) | x <- [2 + i .. 13 - i], y <- [2 + i .. 13 - i] ]
		(PixelArgb32Straight 255 j (255 - j) 0)


