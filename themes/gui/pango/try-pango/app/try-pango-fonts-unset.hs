{-# LANGUAGE LambdaCase #-}
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
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Values

main :: IO ()
main = getArgs >>= \case
	f : stl : v : w : str : _ -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
		cr <- cairoCreate s
		pl <- pangoCairoCreateLayout cr

		fd <- pangoFontDescriptionNew
		case f of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ Family f
		pangoFontDescriptionUnset @Family fd
		print =<< pangoFontDescriptionGet @Family fd
		case stl of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readStyle stl
		pangoFontDescriptionUnset @PangoStyle fd
		print =<< pangoFontDescriptionGet @PangoStyle fd
		case v of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readVariant v
		pangoFontDescriptionUnset @PangoVariant fd
		print =<< pangoFontDescriptionGet @PangoVariant fd
		case w of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readWeight w
		pangoFontDescriptionUnset @PangoWeight fd
		print =<< pangoFontDescriptionGet @PangoWeight fd
		case str of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readStretch str
		pangoFontDescriptionUnset @PangoStretch fd
		print =<< pangoFontDescriptionGet @PangoStretch fd

		pangoLayoutSetFontDescription pl =<< pangoFontDescriptionFreeze fd
		pangoLayoutSetText pl "Hello, world!\nこんにちは、世界!\x1f9a5" 45

		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts-unset.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "no family"

readStyle :: String -> PangoStyle
readStyle "normal" = pangoStyleNormal
readStyle "oblique" = pangoStyleOblique
readStyle "italic" = pangoStyleItalic
readStyle _ = pangoStyleNormal

readVariant :: String -> PangoVariant
readVariant "normal" = pangoVariantNormal
readVariant "small-caps" = pangoVariantSmallCaps
readVariant _ = pangoVariantNormal

readWeight :: String -> PangoWeight
readWeight "thin" = pangoWeightThin
readWeight "ultralight" = pangoWeightUltralight
readWeight "light" = pangoWeightLight
readWeight "semilight" = pangoWeightSemilight
readWeight "book" = pangoWeightBook
readWeight "normal" = pangoWeightNormal
readWeight "medium" = pangoWeightMedium
readWeight "semibold" = pangoWeightSemibold
readWeight "bold" = pangoWeightBold
readWeight "ultrabold" = pangoWeightUltrabold
readWeight "heavy" = pangoWeightHeavy
readWeight "ultraheavy" = pangoWeightUltraheavy
readWeight s
	| all isDigit s = PangoWeight $ read s
	| otherwise = pangoWeightNormal

readStretch :: String -> PangoStretch
readStretch = \case
	"ultra-condensed" -> pangoStretchUltraCondensed
	"extra-condensed" -> pangoStretchExtraCondensed
	"condensed" -> pangoStretchCondensed
	"semi-condensed" -> pangoStretchSemiCondensed
	"normal" -> pangoStretchNormal
	"semi-expanded" -> pangoStretchSemiExpanded
	"expanded" -> pangoStretchExpanded
	"extra-expanded" -> pangoStretchExtraExpanded
	"ultra-expanded" -> pangoStretchUltraExpanded
	_ -> pangoStretchNormal
