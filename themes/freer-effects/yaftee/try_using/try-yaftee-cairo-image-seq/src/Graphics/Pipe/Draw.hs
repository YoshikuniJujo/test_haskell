{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pipe.Draw (
	writeDrawPipe, newImageArgb32Mut, drawColor ) where

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
	Argb32Mut RealWorld -> Eff.E es [Rgba d] o ()
drawColor img = void $
	PipeT.convert ((\(RgbaWord8 r g b a) -> PixelArgb32Straight a r g b) <$>) Pipe.=$=
	draw img 0

drawLine :: ImageMut im => im RealWorld -> CInt -> [PixelMut im] -> IO ()
drawLine img y ps = for_ (zip [0 ..] ps) \(x, p) -> putPixel img x y p

draw :: (
	U.Member Pipe.P es,
	U.Base IO.I es
	) =>
	Argb32Mut RealWorld -> CInt -> Eff.E es [PixelArgb32] o r
draw img y = do
	Eff.effBase @IO . drawLine img y =<< Pipe.await
	draw img (y + 1)
