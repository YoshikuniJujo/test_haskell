{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Char
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations

import System.Environment

import qualified Data.Text as T

main :: IO ()
main = getArgs >>= \case
	opsz : _	| all ((||) <$> isDigit <*> (== '.')) opsz -> do
		s <- cairoImageSurfaceCreate CairoFormatArgb32 300 400
		cr <- cairoCreate s

		fd <- pangoFontDescriptionNew

		pangoFontDescriptionSet fd $ Family "Literata"
		pangoFontDescriptionSet fd $ Size 20
		pangoFontDescriptionSetAxis fd . OpticalSize $ read opsz

		fd' <- pangoFontDescriptionFreeze fd

		print $ pangoFontDescriptionGetAxis @OpticalSize fd'

		pl <- pangoCairoCreateLayout cr
		pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
		pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!"
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts-literata.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "need optical size (7 - 72)"
