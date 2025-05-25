{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pipe.Write (writeDrawPipe) where

import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Surfaces.ImageSurfaces

import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor

writeDrawPipe :: forall sts ->
	HFunctor.Tight (U.U (Append sts '[IO.I])) =>
	FilePath -> Int -> Int -> Argb32Mut RealWorld ->
	(forall r' . Eff.E (sts `Append` '[IO.I]) i o r' -> Eff.E '[IO.I] i o r'') ->
	Eff.E (Pipe.P ': (sts `Append` '[U.FromFirst IO])) i o r -> IO ()
--	Eff.E '[Pipe.P, U.FromFirst IO] i o r -> IO ()
writeDrawPipe sts fp wdt hgt img rn pp = do
	let	img' = CairoImageMutArgb32 img

	_ <- Eff.runM . rn . Pipe.run $ pp

	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 (fromIntegral wdt) (fromIntegral hgt)
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
