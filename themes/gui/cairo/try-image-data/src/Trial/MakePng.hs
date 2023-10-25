{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.MakePng where

import Control.Monad.ST
import Data.Int
import Data.CairoContext
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces

pngWith :: FilePath -> Int32 -> Int32 -> (CairoTIO s -> IO ()) -> IO ()
pngWith fp w h act = do
	sr <- cairoImageSurfaceCreate CairoFormatArgb32 w h
	cr <- cairoCreate sr
	act cr
	makePng sr fp

makePng :: CairoSurfaceImageT s RealWorld -> FilePath -> IO ()
makePng sr fp = cairoImageSurfaceGetCairoImage sr >>= \case
	CairoImageArgb32 ci -> writePng fp $ cairoArgb32ToJuicyRGBA8 ci
	_ -> error "never occur"
