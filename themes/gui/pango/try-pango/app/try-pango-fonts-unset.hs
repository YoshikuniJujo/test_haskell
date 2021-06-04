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
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

main :: IO ()
main = getArgs >>= \case
	f : stl : v : w : str : _ -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
		cr <- cairoCreate s
		pl <- pangoCairoCreateLayout cr

		fd <- pangoFontDescriptionNew
		case f of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ Family f
		pangoFontDescriptionUnset @Family fd
		case stl of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readStyle stl
		pangoFontDescriptionUnset @PangoStyle fd
		case v of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readVariant v
		pangoFontDescriptionUnset @PangoVariant fd
		case w of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readWeight w
		pangoFontDescriptionUnset @PangoWeight fd
		case str of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readStretch str
		pangoFontDescriptionUnset @PangoStretch fd

		fd' <- pangoFontDescriptionFreeze fd

		print $ pangoFontDescriptionGet @Family fd'
		print $ pangoFontDescriptionGet @PangoStyle fd'
		print $ pangoFontDescriptionGet @PangoVariant fd'
		print $ pangoFontDescriptionGet @PangoWeight fd'
		print $ pangoFontDescriptionGet @PangoStretch fd'

		pangoLayoutSetFontDescription pl fd'
		pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!\x1f9a5"

		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts-unset.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "no family"

readStyle :: String -> PangoStyle
readStyle "normal" = PangoStyleNormal
readStyle "oblique" = PangoStyleOblique
readStyle "italic" = PangoStyleItalic
readStyle _ = PangoStyleNormal

readVariant :: String -> PangoVariant
readVariant "normal" = PangoVariantNormal
readVariant "small-caps" = PangoVariantSmallCaps
readVariant _ = PangoVariantNormal

readWeight :: String -> PangoWeight
readWeight "thin" = PangoWeightThin
readWeight "ultralight" = PangoWeightUltralight
readWeight "light" = PangoWeightLight
readWeight "semilight" = PangoWeightSemilight
readWeight "book" = PangoWeightBook
readWeight "normal" = PangoWeightNormal
readWeight "medium" = PangoWeightMedium
readWeight "semibold" = PangoWeightSemibold
readWeight "bold" = PangoWeightBold
readWeight "ultrabold" = PangoWeightUltrabold
readWeight "heavy" = PangoWeightHeavy
readWeight "ultraheavy" = PangoWeightUltraheavy
readWeight s
	| all isDigit s = PangoWeight $ read s
	| otherwise = PangoWeightNormal

readStretch :: String -> PangoStretch
readStretch = \case
	"ultra-condensed" -> PangoStretchUltraCondensed
	"extra-condensed" -> PangoStretchExtraCondensed
	"condensed" -> PangoStretchCondensed
	"semi-condensed" -> PangoStretchSemiCondensed
	"normal" -> PangoStretchNormal
	"semi-expanded" -> PangoStretchSemiExpanded
	"expanded" -> PangoStretchExpanded
	"extra-expanded" -> PangoStretchExtraExpanded
	"ultra-expanded" -> PangoStretchUltraExpanded
	_ -> PangoStretchNormal
