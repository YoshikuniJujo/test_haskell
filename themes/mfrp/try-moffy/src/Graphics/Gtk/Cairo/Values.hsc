{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gtk.Cairo.Values (
	CairoFontSlantT(..),
	cairoFontSlantNormal, cairoFontSlantItalic, cairoFontSlantOblique,
	CairoFontWeightT(..),
	cairoFontWeightNormal, cairoFontWeightBold,
	CairoStatusT(..), cairoStatusSuccess, cairoStatusReadError
	) where

import Data.Word

#include <gtk/gtk.h>

newtype CairoFontSlantT = CairoFontSlantT #{type cairo_font_slant_t} deriving Show

#enum CairoFontSlantT, CairoFontSlantT, \
	CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_SLANT_ITALIC, CAIRO_FONT_SLANT_OBLIQUE

newtype CairoFontWeightT = CairoFontWeightT #{type cairo_font_weight_t} deriving Show

#enum CairoFontWeightT, CairoFontWeightT, \
	CAIRO_FONT_WEIGHT_NORMAL, CAIRO_FONT_WEIGHT_BOLD

newtype CairoStatusT = CairoStatusT #{type cairo_status_t} deriving Show

#enum CairoStatusT, CairoStatusT, CAIRO_STATUS_SUCCESS, CAIRO_STATUS_READ_ERROR
