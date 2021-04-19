{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Foldable
import Data.Maybe
import Data.Color
import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

import Graphics.Cairo.Drawing.Paths

main :: IO ()
main = do
	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSetFamily fd "Source Han Sans VF"
	pangoFontDescriptionSet fd $ Size 20

	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 1120
	cr <- cairoCreate s
	cairoSetSourceRgb cr . fromJust $ rgbDouble 1 1 1
	cairoPaint cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	pl <- pangoCairoCreateLayout cr
	pangoLayoutSetText pl "Hello, world!\nこんにちは、世界!\n" 40
	for_ (zip [0, 80 ..] [250, 300 .. 900]) \(y, w) -> do
		pangoFontDescriptionSetAxis fd $ Weight w
		fd' <- pangoFontDescriptionFreeze fd
		print $ pangoFontDescriptionGetAxis @Weight fd'
		pangoLayoutSetFontDescription pl fd'
		cairoMoveTo cr 0 y
		pangoCairoShowLayout cr pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-fonts-source-han-sans.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
