{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts.PangoFontDescription (
	-- * TYPE
	-- ** PangoFontDescription
	PangoFontDescription,

	-- ** PangoFontDescriptionNullable
	PangoFontDescriptionNullable,
	pangoFontDescriptionFromNullable, pangoFontDescriptionToNullable,

	-- ** PangoFontDescriptionPrim
	PangoFontDescriptionPrim,
	PangoFontDescriptionST, PangoFontDescriptionIO,
	pangoFontDescriptionFreeze, pangoFontDescriptionThaw,
	pangoFontDescriptionCopy,

	-- * FUNCTION
	pangoFontDescriptionNew,
	PangoFontDescriptionSetting,
	pangoFontDescriptionSet, pangoFontDescriptionGet,
	pangoFontDescriptionUnset,
	pangoFontDescriptionMerge, pangoFontDescriptionBetterMatch,
	pangoFontDescriptionToString, pangoFontDescriptionToFilename,

	-- * SETTING
	-- ** Family
	Family(..),

	-- ** Style
	PangoStyle(..),
	pattern PangoStyleNormal, pattern PangoStyleOblique,
	pattern PangoStyleItalic,

	-- ** Variant
	PangoVariant(..),
	pattern PangoVariantNormal, pattern PangoVariantSmallCaps,

	-- ** Weight
	PangoWeight(..),
	pattern PangoWeightThin, pattern PangoWeightUltralight,
	pattern PangoWeightLight, pattern PangoWeightSemilight,
	pattern PangoWeightBook, pattern PangoWeightNormal,
	pattern PangoWeightMedium, pattern PangoWeightSemibold,
	pattern PangoWeightBold, pattern PangoWeightUltrabold,
	pattern PangoWeightHeavy, pattern PangoWeightUltraheavy,

	-- ** Stretch
	PangoStretch(..),
	pattern PangoStretchUltraCondensed, pattern PangoStretchExtraCondensed,
	pattern PangoStretchCondensed, pattern PangoStretchSemiCondensed,
	pattern PangoStretchNormal, pattern PangoStretchSemiExpanded,
	pattern PangoStretchExpanded, pattern PangoStretchExtraExpanded,
	pattern PangoStretchUltraExpanded,

	-- ** Size
	Size(..)

	) where

import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Internal
