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
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Paths.Basic
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 256 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoSet cr $ LineWidth 4

	sample cr False
	cairoTranslate cr 128 0
	sample cr True

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-new-sub-path.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

sample :: PrimMonad m => CairoT r (PrimState m) -> Bool -> m ()
sample cr b = do
	cairoMoveTo cr 32 32
	cairoLineTo cr 96 32
	bool (pure ()) (cairoNewSubPath cr) b
	cairoArc cr 64 64 32 0 (3 / 2 * pi)
	cairoStroke cr
