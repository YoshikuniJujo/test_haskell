{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths.Basic
import Graphics.Cairo.Drawing.Paths.CairoPathT
import Graphics.Cairo.Drawing.Paths.CopyAppend
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 128 128
	cr <- cairoCreate sr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoRectangle cr 32 32 64 64
	CairoPathT pth <- cairoCopyPath cr
	print pth
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.1 0.3 0.05
	cairoSet cr $ LineWidth 8
	cairoSet cr LineCapRound
	cairoMoveTo cr 16 16
	cairoCurveTo cr 112 16 16 112 112 112
	CairoPathT pth' <- cairoCopyPath cr
	CairoPathT pth'' <- cairoCopyPathFlat cr
	print pth'
	print $ length pth''
	print pth''
	cairoStroke cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.3 0.1 0.05
	cairoArc cr 64 64 32 pi (- pi * 1 / 3)
	CairoPathT pth4 <- cairoCopyPath cr
	print $ length pth4
	print pth4
	cairoStroke cr

	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-path-t-exe.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
