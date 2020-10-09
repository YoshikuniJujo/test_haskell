{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Values where

import Data.Word
import Data.Int

#include <cairo.h>

newtype CairoFontSlantT = CairoFontSlantT #{type cairo_font_slant_t} deriving Show

#enum CairoFontSlantT, CairoFontSlantT, CAIRO_FONT_SLANT_NORMAL, \
	CAIRO_FONT_SLANT_ITALIC, CAIRO_FONT_SLANT_OBLIQUE

newtype CairoFontWeightT = CairoFontWeightT #{type cairo_font_weight_t} deriving Show

#enum CairoFontWeightT, CairoFontWeightT, \
	CAIRO_FONT_WEIGHT_NORMAL, CAIRO_FONT_WEIGHT_BOLD

newtype CairoFormatT = CairoFormatT #{type cairo_format_t} deriving Show

#enum CairoFormatT, CairoFormatT, CAIRO_FORMAT_INVALID, CAIRO_FORMAT_ARGB32
