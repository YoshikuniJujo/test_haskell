{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.CairoImage.Internal
import Data.JuicyCairo
import System.Environment
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces

import Text

main :: IO ()
main = getArgs >>= \case
	sz : x : y : _ -> do
		s <- cairoImageSurfaceCreate CairoFormatArgb32 300 400
		cr <- cairoCreate s

		putText cr (Size $ read sz) (read x) (read y) "Hello"

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-simple.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "no family"
