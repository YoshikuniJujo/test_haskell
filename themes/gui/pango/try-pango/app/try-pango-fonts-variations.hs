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

main :: IO ()
main = getArgs >>= \case
	skla : _ -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
		cr <- cairoCreate s

		fd <- pangoFontDescriptionNew

		pangoFontDescriptionSetFamily fd "Decovar Alpha"
		pangoFontDescriptionSet fd $ Size 20
--	pangoFontDescriptionSetVariation fd "SKLA=1000,TRMG=750,WGHT=100"
--	pangoFontDescriptionSetVariation fd "TRMG=750"
--	pangoFontDescriptionSetVariation fd "TRMG=1000"
		pangoFontDescriptionSetAxis fd . InlineSkeleton $ read skla

		pl <- pangoCairoCreateLayout cr
		pangoLayoutSetFontDescription pl fd
		pangoLayoutSetText pl "Hello, world!\nこんにちは、世界!" 40
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts-variations.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "need skla"

newtype InlineSkeleton = InlineSkeleton { getInlineSkeleton :: Double } deriving Show

instance PangoFontDescriptionAxis InlineSkeleton where
	pangoFontDescriptionAxisTag = "SKLA"
	pangoFontDescriptionAxisToDouble = getInlineSkeleton
	pangoFontDescriptionAxisFromDouble = InlineSkeleton
