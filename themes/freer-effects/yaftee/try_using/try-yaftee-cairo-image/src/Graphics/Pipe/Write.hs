{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pipe.Write (writeDrawPipe) where

import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.HigherOpenUnion qualified as U
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Surfaces.ImageSurfaces

writeDrawPipe :: FilePath ->
	Argb32Mut RealWorld -> Eff.E '[Pipe.P, U.FromFirst IO] i o a -> IO ()
writeDrawPipe fp img pp = do
	let	img' = CairoImageMutArgb32 img

	_ <- Eff.runM . Pipe.run $ pp

	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 16 16
	cr <- cairoCreate sfc0

	sfc <- CairoSurfaceTImage <$> cairoImageSurfaceCreateForCairoImageMut img'
	ptn <- cairoPatternCreateForSurface sfc
	cairoSetSource cr ptn
	cairoPaint cr

	cairoImageSurfaceGetCairoImageMut sfc0 >>= \case
		CairoImageMutArgb32 i -> writeArgb32Mut fp i
		_ -> error "bad"

writeArgb32Mut :: FilePath -> Argb32Mut RealWorld -> IO ()
writeArgb32Mut fp = (writePng fp =<<) . cairoArgb32MutToJuicyRGBA8
