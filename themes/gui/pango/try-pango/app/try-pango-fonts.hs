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
import Graphics.Pango.Basic.VerticalText
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

main :: IO ()
main = getArgs >>= \case
	f : stl : v : w : str : sz : gr : _ -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
		cr <- cairoCreate s
		pl <- pangoCairoCreateLayout cr

		fd <- pangoFontDescriptionPrimNew
		case f of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ Family f
		case stl of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readStyle stl
		case v of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readVariant v
		case w of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readWeight w
		case str of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readStretch str
		case sz of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readSize sz
		case gr of "-" -> pure (); _ -> pangoFontDescriptionSet fd $ readGravity gr

		fd' <- pangoFontDescriptionFreeze fd

		print $ pangoFontDescriptionGet @Family fd'
		print $ pangoFontDescriptionGet @PangoStyle fd'
		print $ pangoFontDescriptionGet @PangoVariant fd'
		print $ pangoFontDescriptionGet @PangoWeight fd'
		print $ pangoFontDescriptionGet @PangoStretch fd'
		print $ pangoFontDescriptionGet @Size fd'
		print $ pangoFontDescriptionGet @PangoGravity fd'

		print $ pangoFontDescriptionToString fd'
		print $ pangoFontDescriptionToFilename fd'

		pangoLayoutSetFontDescription pl fd'
		pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!\n\x1f9a5"

		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts.png" $ cairoArgb32ToJuicyRGBA8 a
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

readSize :: String -> Size
readSize = \case
	'a' : sz | all isDigit sz -> AbsoluteSize $ read sz
	sz | all isDigit sz -> Size $ read sz
	_ -> Size 20

readGravity :: String -> PangoGravity
readGravity = \case
	"south" -> PangoGravitySouth
	"east" -> PangoGravityEast
	"north" -> PangoGravityNorth
	"west" -> PangoGravityWest
	"auto" -> PangoGravityAuto
	_ -> PangoGravityAuto
