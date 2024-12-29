{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Primitive
import Data.Foldable
import Data.Maybe
import Data.CairoContext
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.SaveAndRestore
import Graphics.Cairo.Drawing.CairoT.Clip
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.CairoT.CairoOperatorT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 512 1936
	cr <- cairoCreate sr

	cairoTranslate cr 16 16
	traverse_ (\o -> sample cr o >> cairoTranslate cr 0 128)  [
		OperatorClear,
		OperatorSource, OperatorOver, OperatorIn, OperatorOut, OperatorAtop,
		OperatorDest, OperatorDestOver, OperatorDestIn, OperatorDestOut, OperatorDestAtop,
		OperatorXor, OperatorAdd, OperatorSaturate ]

	cairoIdentityMatrix cr
	cairoTranslate cr 272 16
	traverse_ (\o -> sample cr o >> cairoTranslate cr 0 128)  [
		OperatorMultiply, OperatorScreen, OperatorOverlay, OperatorDarken, OperatorLighten,
		OperatorColorDodge, OperatorColorBurn, OperatorHardLight, OperatorSoftLight,
		OperatorDifference, OperatorExclusion,
		OperatorHslHue, OperatorHslSaturation, OperatorHslColor, OperatorHslLuminosity ]

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-operator.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

sample :: PrimMonad m => CairoT r (PrimState m) -> Operator -> m ()
sample cr o = do
	cairoSave cr
	cairoRectangle cr 0 0 256 128
	cairoClip cr

	cairoSet cr OperatorOver
	cairoRectangle cr 0 0 120 90
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0.7 0 0 0.8
	cairoFill cr

	cairoSet cr o

	cairoRectangle cr 40 30 120 90
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0 0 0.9 0.4
	cairoFill cr

	cairoRestore cr
