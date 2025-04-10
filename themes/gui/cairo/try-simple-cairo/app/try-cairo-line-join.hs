{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

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
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 128 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoSet cr $ LineWidth 12

	print =<< cairoGet @LineJoin cr
	sampleLines cr

	cairoTranslate cr 0 40
	cairoSet cr $ LineJoinRound
	print =<< cairoGet @LineJoin cr
	sampleLines cr

	cairoTranslate cr 0 40
	cairoSet cr $ LineJoinBevel
	print =<< cairoGet @LineJoin cr
	sampleLines cr

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-line-join.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"

sampleLines :: PrimMonad m => CairoT r (PrimState m) -> m ()
sampleLines cr = do
	cairoMoveTo cr 4 16
	cairoLineTo cr 88 16
	cairoLineTo cr 40 32
	cairoStroke cr
