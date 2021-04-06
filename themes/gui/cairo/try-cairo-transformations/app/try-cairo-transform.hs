{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.Color
import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Utilities.CairoMatrixT

import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 128 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoSet cr $ LineWidth 4
	cairoSet cr LineJoinRound

	mt <- cairoMatrixNewTranslate 64 64
	cairoMatrixScale mt (1 / 2) 1
	cairoMatrixRotate mt (pi / 4)
	cairoMatrixTranslate mt (- 64) (- 64)
	cairoTransform cr mt
	print =<< cairoMatrixGet =<< cairoGetMatrix cr
	print =<< cairoUserToDevice cr 10 100
	print =<< cairoDeviceToUser cr 32.1801 51.2720
	print =<< cairoUserToDeviceDistance cr 10 100
	print =<< cairoDeviceToUserDistance cr (- 31.8198) 77.7817

	cairoRectangle cr 32 32 64 64
	cairoStroke cr
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-transform.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
