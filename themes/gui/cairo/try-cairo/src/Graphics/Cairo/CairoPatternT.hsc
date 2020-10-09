{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.CairoPatternT (
	cairoPatternAddColorStopRgb, cairoPatternAddColorStopRgba,
	cairoPatternCreateLinear, cairoPatternCreateRadial
	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)

import Graphics.Cairo.Types

#include <cairo.h>

foreign import ccall "cairo_pattern_add_color_stop_rgb" c_cairo_pattern_add_color_stop_rgb ::
	Ptr CairoPatternT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoPatternAddColorStopRgb ::
	CairoPatternT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoPatternAddColorStopRgb (CairoPatternT fpt) os r g b =
	withForeignPtr fpt \pt ->
		c_cairo_pattern_add_color_stop_rgb pt os r g b

foreign import ccall "cairo_pattern_add_color_stop_rgba" c_cairo_pattern_add_color_stop_rgba ::
	Ptr CairoPatternT -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoPatternAddColorStopRgba ::
	CairoPatternT -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoPatternAddColorStopRgba (CairoPatternT fpt) os r g b a =
	withForeignPtr fpt \pt ->
		c_cairo_pattern_add_color_stop_rgba pt os r g b a

foreign import ccall "cairo_pattern_create_linear" c_cairo_pattern_create_linear ::
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> IO (Ptr CairoPatternT)

cairoPatternCreateLinear ::
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> IO CairoPatternT
cairoPatternCreateLinear x0 y0 x1 y1 =
	makeCairoPatternT =<< c_cairo_pattern_create_linear x0 y0 x1 y1

foreign import ccall "cairo_pattern_create_radial" c_cairo_pattern_create_radial ::
	#{type double} -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> IO (Ptr CairoPatternT)

cairoPatternCreateRadial ::
	#{type double} -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> IO CairoPatternT
cairoPatternCreateRadial cx0 cy0 r0 cx1 cy1 r1 =
	makeCairoPatternT =<< c_cairo_pattern_create_radial cx0 cy0 r0 cx1 cy1 r1
