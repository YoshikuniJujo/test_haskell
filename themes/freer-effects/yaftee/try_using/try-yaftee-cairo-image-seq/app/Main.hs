{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Control.Monad
import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Surfaces.ImageSurfaces

import Data.Time
import Data.Color

main :: IO ()
main = do
	img <- newImageMut @Argb32Mut 16 16
	writeDrawPipe "foobar.png" 16 16 examplePipe img

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

examplePipe img =
	void $ Eff.runM . Pipe.run
		$ samplePipe Pipe.=$= drawColor img

drawColor :: (RealFrac d, U.Member Pipe.P es, U.Base IO.I es) =>
	Argb32Mut RealWorld -> Eff.E es [Rgba d] o ()
drawColor img = void $
	PipeT.convert ((\(RgbaWord8 r g b a) -> PixelArgb32Straight a r g b) <$>) Pipe.=$=
	draw img 0

samplePipe :: (
	RealFrac d,
	U.Member Pipe.P es
	) =>
	Eff.E es i [Rgba d] ()
samplePipe = (`mapM_` [0 .. 15]) \y -> Pipe.yield ((\x ->
	RgbaWord8 (x * 15 + 30) (y * 15 + 30) ((225 - x * 15 `div` 2 - y * 15 `div` 2) * 2 `div` 3) 255) <$> [0 .. 15])

drawLine img y ps = for_ (zip [0 ..] ps) \(x, p) -> putPixel img x y p

draw :: (
	U.Member Pipe.P es,
	U.Base IO.I es
	) =>
	Argb32Mut RealWorld -> CInt -> Eff.E es [PixelArgb32] o r
draw img y = do
	Eff.effBase @IO . drawLine img y =<< Pipe.await
	draw img (y + 1)
