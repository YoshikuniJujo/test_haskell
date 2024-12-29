{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Primitive
import Data.Bool
import Data.Maybe
import Data.CairoContext
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths.Basic
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 256 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoSet cr $ LineWidth 8

	sample cr False
	cairoTranslate cr 128 0
	sample cr True

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-arc.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

sample :: PrimMonad m => CairoT r (PrimState m) -> Bool -> m ()
sample cr b = do
	bool cairoArc cairoArcNegative b cr 64 64 32 (pi / 4) pi
	cairoStroke cr
