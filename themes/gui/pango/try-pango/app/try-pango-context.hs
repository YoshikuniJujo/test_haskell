{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.Rendering
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo
import Graphics.Pango.Values

import Data.CairoImage
import Data.JuicyCairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
	cr <- cairoCreate s
	cxt <- pangoCairoCreateContext cr

	pangoContextSetFontDescription cxt PangoFontDescriptionNull

	pl <- pangoLayoutNew cxt

	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSetSize pfd (30 * pangoScale)
	pangoLayoutSetFontDescription pl =<< pangoFontDescriptionFreeze pfd

	pangoLayoutSet @T.Text pl "こんにちは世界!"
	pangoCairoShowLayout cr pl
--	void $ writeDynamicPng "tmp2.png" =<< cairoImageSurfaceGetImage s
	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-context.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
