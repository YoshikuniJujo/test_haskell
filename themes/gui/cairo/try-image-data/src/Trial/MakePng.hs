{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.MakePng where

import Data.Int
import Data.CairoContext
import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

pngWith :: FilePath -> Int32 -> Int32 -> (CairoTIO s -> IO ()) -> IO ()
pngWith fp w h act = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 w h
	cr <- cairoCreate sr
	act cr
	cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci ->
			writePng fp $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur"
