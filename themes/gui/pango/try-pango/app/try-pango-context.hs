{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Rendering.Cairo
import Graphics.Pango.Values

import Data.CairoImage
import Data.JuicyCairo

import Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
	cr <- cairoCreate s
	pc <- pangoCairoCreateContext cr
	pl <- pangoLayoutNew pc
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSetSize pfd (30 * pangoScale)
	pangoLayoutSetFontDescription pl pfd
	pangoLayoutSetText pl "こんにちは世界!" 30
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
--	void $ writeDynamicPng "tmp2.png" =<< cairoImageSurfaceGetImage s
	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "tmp2.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
