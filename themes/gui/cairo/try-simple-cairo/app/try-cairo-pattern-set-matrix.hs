{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Drawing.CairoPatternT.Setting
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Utilities.CairoMatrixT
-- import Graphics.Cairo.Values

main :: IO ()
main = readImage "data/HaskellLogo.png" >>= \case
	Right (ImageRGBA8 i) -> do
		sr1 <- cairoImageSurfaceCreateForCairoImage . CairoImageArgb32
			$ juicyRGBA8ToCairoArgb32 i
		pt <- cairoPatternCreateForSurface sr1
		mtx <- cairoMatrixNewTranslate @_ @CairoMatrixT 64 64
		cairoMatrixRotate mtx (pi / 4)
		cairoMatrixTranslate mtx (- 64) (- 64)
		cairoMatrixTranslate mtx (- 64) (- 64)
		cairoPatternSetMatrix pt mtx

		print =<< cairoMatrixGet =<< cairoPatternGetMatrix pt

		sr <- cairoImageSurfaceCreate CairoFormatArgb32 256 256
		cr <- cairoCreate sr
		cairoSetSource cr pt
		cairoPaint cr
		cairoImageSurfaceGetCairoImage sr >>= \case
			CairoImageArgb32 ci ->
				writePng "try-cairo-pattern-set-matrix.png" $ cairoArgb32ToJuicyRGBA8 ci
			_ -> error "never occur"
	_ -> error "bad"
