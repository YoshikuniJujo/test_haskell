{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Lib

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 128 128
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoRectangle cr 32 32 64 64
	pth <- cairoCopyPath cr
	print pth
	print =<< withCairoPathT pth cairoPathTPathList
	cairoFill cr
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-path-t-exe.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
