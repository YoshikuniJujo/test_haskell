{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Values where

import Data.Word

#include <pango/pango.h>

pangoScale :: Num n => n
pangoScale = #const PANGO_SCALE

newtype PangoVariant = PangoVariant #{type PangoVariant} deriving Show

#enum PangoVariant, PangoVariant, PANGO_VARIANT_NORMAL, PANGO_VARIANT_SMALL_CAPS

newtype PangoWeight = PangoWeight #{type PangoWeight} deriving Show

#enum PangoWeight, PangoWeight, PANGO_WEIGHT_THIN, PANGO_WEIGHT_ULTRALIGHT, \
	PANGO_WEIGHT_LIGHT, PANGO_WEIGHT_SEMILIGHT, PANGO_WEIGHT_BOOK, \
	PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_MEDIUM, PANGO_WEIGHT_SEMIBOLD, \
	PANGO_WEIGHT_BOLD, PANGO_WEIGHT_ULTRABOLD, PANGO_WEIGHT_HEAVY, \
	PANGO_WEIGHT_ULTRAHEAVY

newtype PangoStretch = PangoStretch #{type PangoStretch} deriving Show

#enum PangoStretch, PangoStretch, PANGO_STRETCH_ULTRA_CONDENSED, \
	PANGO_STRETCH_EXTRA_CONDENSED, PANGO_STRETCH_CONDENSED, \
	PANGO_STRETCH_SEMI_CONDENSED, PANGO_STRETCH_NORMAL, \
	PANGO_STRETCH_SEMI_EXPANDED, PANGO_STRETCH_EXPANDED, \
	PANGO_STRETCH_EXTRA_EXPANDED, PANGO_STRETCH_ULTRA_EXPANDED

newtype PangoFontMask = PangoFontMask #{type PangoFontMask} deriving Show

#enum PangoFontMask, PangoFontMask, PANGO_FONT_MASK_FAMILY, \
	PANGO_FONT_MASK_STYLE, PANGO_FONT_MASK_VARIANT, \
	PANGO_FONT_MASK_WEIGHT, PANGO_FONT_MASK_STRETCH, \
	PANGO_FONT_MASK_SIZE, PANGO_FONT_MASK_GRAVITY, \
	PANGO_FONT_MASK_VARIATIONS

newtype PangoEllipsizeMode = PangoEllipsizeMode #{type PangoEllipsizeMode} deriving Show

#enum PangoEllipsizeMode, PangoEllipsizeMode, PANGO_ELLIPSIZE_NONE, \
	PANGO_ELLIPSIZE_START, PANGO_ELLIPSIZE_MIDDLE, PANGO_ELLIPSIZE_END

newtype PangoAlignment = PangoAlignment #{type PangoAlignment} deriving Show

#enum PangoAlignment, PangoAlignment, PANGO_ALIGN_LEFT, PANGO_ALIGN_CENTER, \
	PANGO_ALIGN_RIGHT

newtype PangoTabAlign = PangoTabAlign #{type PangoTabAlign} deriving Show


#enum PangoTabAlign, PangoTabAlign, PANGO_TAB_LEFT
