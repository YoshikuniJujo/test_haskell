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
import Graphics.Cairo.Drawing.CairoT.Basic
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 256 128
	cr <- cairoCreate sr

	sample cr False

	cairoTranslate cr 128 0
	sample cr True

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-new-path.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

sample :: PrimMonad m => CairoT r (PrimState m) -> Bool -> m ()
sample cr b = do
	cairoRectangle cr 16 16 64 32
	bool (pure ()) (cairoNewPath cr) b
	cairoRectangle cr 32 32 64 64
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoFillPreserve cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.3 0.05
	cairoStroke cr
