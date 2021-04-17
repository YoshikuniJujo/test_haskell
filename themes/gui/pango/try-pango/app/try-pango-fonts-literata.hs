{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Char
import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

import System.Environment

import qualified Data.Map as M

main :: IO ()
main = getArgs >>= \case
	opsz : _	| all ((||) <$> isDigit <*> (== '.')) opsz -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
		cr <- cairoCreate s

		fd <- pangoFontDescriptionNew

		pangoFontDescriptionSetFamily fd "Literata"
		pangoFontDescriptionSet fd $ Size 20
		pangoFontDescriptionSetAxis fd . OpticalSize $ read opsz
		print =<< pangoFontDescriptionGetAxis @OpticalSize fd


		pl <- pangoCairoCreateLayout cr
		pangoLayoutSetFontDescription pl =<< pangoFontDescriptionFreeze fd
		pangoLayoutSetText pl "Hello, world!\nこんにちは、世界!" 40
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts-literata.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "need optical size (7 - 72)"
