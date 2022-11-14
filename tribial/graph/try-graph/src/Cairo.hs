{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Cairo (withCairo) where

import Control.Monad.ST
import Data.Maybe
import Data.Int
import Data.Color
import Data.CairoContext
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces

withCairo :: FilePath -> Int32 -> Int32 -> (CairoT s RealWorld -> IO a) -> IO a
withCairo fp w h f = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 w h
	cr <- cairoCreate sr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 1.0 1.0 1.0
	cairoPaint cr
	f cr <* (cairoImageSurfaceGetCairoImage sr >>= \case
		CairoImageArgb32 ci -> writePng fp $ cairoArgb32ToJuicyRGBA8 ci
		_ -> error "never occur")
