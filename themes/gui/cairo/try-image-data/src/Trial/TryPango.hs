{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryPango where

import Foreign.C.Types
import Data.CairoContext

import Graphics.Cairo.Drawing.Paths
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

import Data.ImageData.Text

drawFont :: CairoTIO s -> CDouble -> CDouble -> Font -> T.Text -> IO ()
drawFont cr x y fnt t = do
	pl <- pangoCairoCreateLayout cr

	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd . Family $ getFontFamily fnt
	pangoFontDescriptionSet fd $ getFontSize fnt
	case fnt of
		Font {	fontStyle = s, fontVariant = v, fontWeight = w,
			fontStretch = st } -> do
			pangoFontDescriptionSet fd $ toStyle s
			pangoFontDescriptionSet fd $ toVariant v
			pangoFontDescriptionSet fd $ toWeight w
			pangoFontDescriptionSet fd $ toStretch st
		_ -> pure ()
	fd' <- pangoFontDescriptionFreeze fd

	cairoMoveTo cr x y
	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet @T.Text pl t

	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

getFontFamily :: Font -> String
getFontFamily = \case
	Font { fontFamily = ff } -> ff
	VariableFont { variableFontFamily = ff } -> ff

getFontSize :: Font -> Size
getFontSize = \case
	Font { fontSize = s } -> toSize s
	VariableFont { variableFontSize = s } -> toSize s

toSize :: FontSize -> Size
toSize (FontSize s) = Size $ realToFrac s
toSize (AbsoluteFontSize s) = AbsoluteSize $ realToFrac s

toStyle :: FontStyle -> PangoStyle
toStyle = \case
	StyleNormal -> PangoStyleNormal
	StyleOblique -> PangoStyleOblique
	StyleItalic -> PangoStyleItalic

toVariant :: FontVariant -> PangoVariant
toVariant = \case
	VariantNormal -> PangoVariantNormal
	VariantSmallCaps -> PangoVariantSmallCaps

toWeight :: FontWeight -> PangoWeight
toWeight = \case
	WeightThin -> PangoWeightThin
	WeightUltralight -> PangoWeightUltralight
	WeightLight -> PangoWeightLight
	WeightSemilight -> PangoWeightSemilight
	WeightBook -> PangoWeightBook
	WeightNormal -> PangoWeightNormal
	WeightMedium -> PangoWeightMedium
	WeightSemibold -> PangoWeightSemibold
	WeightBold -> PangoWeightBold
	WeightUltrabold -> PangoWeightUltrabold
	WeightHeavy -> PangoWeightHeavy
	WeightUltraheavy -> PangoWeightUltraheavy

toStretch :: FontStretch -> PangoStretch
toStretch = \case
	StretchUltraCondensed -> PangoStretchUltraCondensed
	StretchExtraCondensed -> PangoStretchExtraCondensed
	StretchCondensed -> PangoStretchCondensed
	StretchSemiCondensed -> PangoStretchSemiCondensed
	StretchNormal -> PangoStretchNormal
	StretchSemiExpanded -> PangoStretchSemiExpanded
	StretchExpanded -> PangoStretchExpanded
	StretchExtraExpanded -> PangoStretchExtraExpanded
	StretchUltraExpanded -> PangoStretchUltraExpanded
