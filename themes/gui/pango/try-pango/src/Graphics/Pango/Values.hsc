{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Values where

import Data.Word

#include <pango/pango.h>

pangoScale :: Num n => n
pangoScale = #const PANGO_SCALE

newtype PangoStyle = PangoStyle #{type PangoStyle} deriving Show

#enum PangoStyle, PangoStyle, PANGO_STYLE_NORMAL, PANGO_STYLE_OBLIQUE, \
	PANGO_STYLE_ITALIC

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
	PANGO_STRETCH_SEMI_EXPANDED, PANGO_STRETCH_EXTRA_EXPANDED, \
	PANGO_STRETCH_ULTRA_EXPANDED
