{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pipe.Draw (
	writeDrawPipe, newImageArgb32Mut, drawColor, drawColor' ) where

import Foreign.C.Types
import Control.Arrow
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

import Data.Color

writeDrawPipe :: FilePath ->
	Argb32Mut RealWorld ->
	Int -> Int -> (Argb32Mut RealWorld -> IO ()) -> IO ()
writeDrawPipe fp img wdt hgt act = do
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

newImageArgb32Mut :: Int -> Int -> IO (Argb32Mut RealWorld)
newImageArgb32Mut = curry
	$ uncurry (newImageMut @Argb32Mut) . (fromIntegral *** fromIntegral)

writeArgb32Mut :: FilePath -> Argb32Mut RealWorld -> IO ()
writeArgb32Mut fp = (writePng fp =<<) . cairoArgb32MutToJuicyRGBA8

drawColor :: (RealFrac d, U.Member Pipe.P es, U.Base IO.I es) =>
	Argb32Mut RealWorld -> [Int] -> [[Int]] -> Eff.E es [Rgba d] o ()
drawColor img ys xss = void $
	PipeT.convert ((\(RgbaWord8 r g b a) -> PixelArgb32Straight a r g b) <$>) Pipe.=$=
	draw img (fromIntegral <$> ys) ((fromIntegral <$>) <$> xss)

drawColor' :: (RealFrac d, U.Member Pipe.P es, U.Base IO.I es) =>
	Argb32Mut RealWorld -> [(Int, (Int, Int))] -> [[Int]] -> IO a -> Eff.E es [Rgba d] o ()
drawColor' img ys xss act = void $
	PipeT.convert ((\(RgbaWord8 r g b a) -> PixelArgb32Straight a r g b) <$>) Pipe.=$=
	draw' img ((\(y, (w, h)) -> (fromIntegral y, (fromIntegral w, fromIntegral h))) <$> ys) ((fromIntegral <$>) <$> xss) act

drawLine :: ImageMut im => im RealWorld -> CInt -> [CInt] -> [PixelMut im] -> IO ()
drawLine img y xs ps = for_ (zip xs ps) \(x, p) -> putPixel img x y p

drawLine' :: ImageMut im => im RealWorld -> CInt -> [CInt] -> (CInt, CInt) -> [PixelMut im] -> IO ()
drawLine' img y xs (w, h) ps = for_ (zip xs ps) \(x, p) -> putBlock img x y w h p

putBlock img x0 y0 w h p = for_ [y0 .. y0 + h - 1] \y ->
	for_ [x0 .. x0 + w - 1] \x -> putPixel img x y p

draw :: (
	U.Member Pipe.P es,
	U.Base IO.I es
	) =>
	Argb32Mut RealWorld -> [CInt] -> [[CInt]] -> Eff.E es [PixelArgb32] o r
draw img (y : ys) (xs : xss) = do
	Eff.effBase @IO . drawLine img y xs =<< Pipe.await
	draw img ys xss
draw _ [] _ = error "no more y positions"
draw _ _ [] = error "no more x positions"

draw' :: (
	U.Member Pipe.P es,
	U.Base IO.I es
	) =>
	Argb32Mut RealWorld -> [(CInt, (CInt, CInt))] -> [[CInt]] -> IO a -> Eff.E es [PixelArgb32] o r
draw' img ((y, (w, h)) : ys) (xs : xss) act = do
	Eff.effBase @IO . drawLine' img y xs (w, h) =<< Pipe.await
	_ <- Eff.effBase act
	draw' img ys xss act
draw' _ [] _ _ = error "no more y positions"
draw' _ _ [] _ = error "no more x positions"
