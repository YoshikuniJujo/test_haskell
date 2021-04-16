{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import System.Environment
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

pangoFontDescriptionAddAxis "Inline" "BLDA"
pangoFontDescriptionAddAxis "Worm" "BLDB"
pangoFontDescriptionAddAxis "InlineSkeleton" "SKLA"

main :: IO ()
main = getArgs >>= \case
	blda : bldb : skla : _ -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 800 400
		cr <- cairoCreate s

		fd <- pangoFontDescriptionNew

		pangoFontDescriptionSetFamily fd "Decovar Alpha"
		pangoFontDescriptionSet fd $ Size 80
		case blda of
			"-" -> pure ()
			_ -> pangoFontDescriptionSetAxis fd . Inline $ read blda
		case bldb of
			"-" -> pure ()
			_ -> pangoFontDescriptionSetAxis fd . Worm $ read bldb
		case skla of
			"-" -> pure ()
			_ -> pangoFontDescriptionSetAxis fd . InlineSkeleton $ read skla

		pl <- pangoCairoCreateLayout cr
		pangoLayoutSetFontDescription pl fd
		pangoLayoutSetText pl "Hello, world!\nこんにちは世界!" 40
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts-variations.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "need blda and skla"
