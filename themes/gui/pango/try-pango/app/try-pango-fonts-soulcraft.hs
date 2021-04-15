{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

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

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
	cr <- cairoCreate s

	fd <- pangoFontDescriptionNew

	pangoFontDescriptionSetFamily fd "Soulcraft"
	pangoFontDescriptionSet fd $ Size 20
--	pangoFontDescriptionSetVariation fd "SKLA=1000,TRMG=750,WGHT=100"
--	pangoFontDescriptionSetVariation fd "TRMG=750"
	pangoFontDescriptionSetVariation fd "wght=500,slnt=40,wdth=200"

	pl <- pangoCairoCreateLayout cr
	pangoLayoutSetFontDescription pl fd
	pangoLayoutSetText pl "Hello, world!\nこんにちは、世界!" 40
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-fonts-soulcraft.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
