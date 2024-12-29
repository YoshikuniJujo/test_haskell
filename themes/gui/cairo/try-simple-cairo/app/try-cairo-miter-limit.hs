{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad.Primitive
import Data.Maybe
import Data.CairoContext
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 240 160
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoSet cr $ LineWidth 10

	sampleLines cr 64 32

	cairoTranslate cr 0 40
	sampleLines cr 60 32

	cairoTranslate cr 0 40
	sampleLines cr 40 24

	cairoTranslate cr 0 40
	sampleLines cr 20 24

	cairoSet cr $ MiterLimit 2.0

	cairoIdentityMatrix cr
	cairoTranslate cr 128 0
	sampleLines cr 64 32

	cairoTranslate cr 0 40
	sampleLines cr 60 32

	cairoTranslate cr 0 40
	sampleLines cr 40 24

	cairoTranslate cr 0 40
	sampleLines cr 20 24

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-miter-limit.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

sampleLines :: PrimMonad m => CairoT r (PrimState m) -> CDouble -> CDouble -> m ()
sampleLines cr x y = do
	cairoMoveTo cr 16 16
	cairoLineTo cr 88 16
	cairoLineTo cr (x + 16) y
	cairoStroke cr
