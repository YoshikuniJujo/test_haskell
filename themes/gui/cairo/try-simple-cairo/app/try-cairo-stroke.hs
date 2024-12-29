{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.Color
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT.Basic
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

main :: IO ()
main = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 128 128
	cr <- cairoCreate sr
	print =<< cairoGet @LineWidth cr
	cairoSet cr $ LineWidth 3
	print =<< cairoGet @LineWidth cr
	print =<< cairoGet @Dash cr
	cairoSet cr $ Dash [8, 32, 7] 3
	print =<< cairoGet @Dash cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.6 0.1
	cairoRectangle cr 32 32 64 64
	print =<< cairoStrokeExtents cr
	CairoExtentsLeftTopWidthHeight {
		cairoExtentsLeft = l, cairoExtentsTop = t, cairoExtentsWidth = w, cairoExtentsHeight = h } <- cairoStrokeExtents cr
	print (l, t, w, h)
	print =<< cairoInStroke cr 32 48
	print =<< cairoInStroke cr 58 96
	print =<< cairoInStroke cr 47 59
	cairoStroke cr
	print =<< cairoGet @Dash cr
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng "try-cairo-stroke.png" $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
