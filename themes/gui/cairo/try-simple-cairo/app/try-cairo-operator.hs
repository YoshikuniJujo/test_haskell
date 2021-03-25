{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Primitive
import Data.Maybe
import Data.CairoContext
import Data.CairoImage
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Clip
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.CairoT.CairoOperatorT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 256 1024
	cr <- cairoCreate sr

	cairoTranslate cr 16 16
	sample cr OperatorClear

	cairoTranslate cr 0 128
	sample cr OperatorSource

	cairoTranslate cr 0 128
	sample cr OperatorOver

	cairoTranslate cr 0 128
	cairoRectangle cr 0 0 128 128
	cairoClip cr
	sample cr OperatorIn
	cairoResetClip cr

	cairoTranslate cr 0 128
	sample cr OperatorOver

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-operator.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

sample :: PrimMonad m => CairoT (PrimState m) -> Operator -> m ()
sample cr o = do
	cairoSet cr OperatorOver
	cairoRectangle cr 0 0 120 90
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0.7 0 0 0.8
	cairoFill cr

	cairoSet cr o

	cairoRectangle cr 40 30 120 90
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0 0 0.9 0.4
	cairoFill cr
