{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Draw (withPng) where

import Control.Monad.ST
import Data.Int
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.CairoContext
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces

withPng :: FilePath -> Int32 -> Int32 -> (CairoT r RealWorld -> IO a) -> IO ()
withPng fp w h f = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 w h
	cr <- cairoCreate s
	_ <- f cr
	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng fp $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
