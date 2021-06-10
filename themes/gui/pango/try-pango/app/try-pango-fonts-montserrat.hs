{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Char
import Data.CairoImage
import Data.JuicyCairo
import System.Environment
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

import qualified Data.Text as T

main :: IO ()
main = getArgs >>= \case
	wght : _	| all ((||) <$> isDigit <*> (== '.')) wght -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
		cr <- cairoCreate s

		fd <- pangoFontDescriptionPrimNew

		pangoFontDescriptionSetFamily fd "Montserrat"
		pangoFontDescriptionSet fd $ Size 20
		pangoFontDescriptionSetAxis fd . Weight $ read wght

		fd' <- pangoFontDescriptionFreeze fd
		print $ pangoFontDescriptionGetAxis @Weight fd'

		pl <- pangoCairoCreateLayout cr
		pangoLayoutSetFontDescription pl fd'
		pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!"
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts-montserrat.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "need weight (100 - 900)"
