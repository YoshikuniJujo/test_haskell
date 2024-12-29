{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Primitive
import Data.Maybe
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.CairoContext
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
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 128 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoSet cr $ LineWidth 16

	sampleLine cr

	cairoTranslate cr 0 32
	cairoSet cr LineCapRound
	sampleLine cr

	cairoTranslate cr 0 32
	cairoSet cr LineCapSquare
	sampleLine cr

	print =<< cairoGet @LineCap cr

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-line-cap.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

sampleLine :: PrimMonad m => CairoT r (PrimState m) -> m ()
sampleLine cr = do
	cairoMoveTo cr 16 32
	cairoLineTo cr 112 32
	cairoStroke cr
