{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Primitive
import Data.Maybe
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.CairoContext
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT.Basic
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 256 256
	cr <- cairoCreate sr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.3 0.05
	cairoTranslate cr 64 0
	figure cr
	cairoStroke cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoIdentityMatrix cr
	cairoTranslate cr 0 128
	figure cr
	cairoSet cr FillRuleEvenOdd
	cairoFillPreserve cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.3 0.05
	cairoStroke cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoTranslate cr 128 0
	figure cr
	cairoSet cr FillRuleWinding
	cairoFillPreserve cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.3 0.05
	cairoStroke cr

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-fill-rule.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

figure :: PrimMonad m => CairoT r (PrimState m) -> m ()
figure cr = do
	cairoMoveTo cr 10 70
	cairoLineTo cr 50 100
	cairoLineTo cr 80 40
	cairoLineTo cr 50 10
	cairoLineTo cr 50 60
	cairoLineTo cr 120 65
	cairoLineTo cr 120 95
	cairoLineTo cr 60 85
	cairoLineTo cr 30 65
	cairoLineTo cr 30 30
	cairoLineTo cr 95 30
	cairoLineTo cr 100 80
	cairoLineTo cr 45 123
	cairoClosePath cr
