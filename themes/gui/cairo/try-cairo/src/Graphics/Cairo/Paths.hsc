{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Paths (
	cairoClosePath, cairoArc, cairoLineTo, cairoMoveTo, cairoRectangle,
	cairoRelCurveTo, cairoRelLineTo
	) where

import Foreign.Ptr
import Foreign.ForeignPtr

import Graphics.Cairo.Types

#include <cairo.h>

cairoMoveTo, cairoLineTo, cairoRelLineTo :: CairoT -> #{type double} -> #{type double} -> IO ()
cairoMoveTo (CairoT fcr) x y = withForeignPtr fcr \cr -> c_cairo_move_to cr x y
cairoLineTo (CairoT fcr) x y = withForeignPtr fcr \cr -> c_cairo_line_to cr x y
cairoRelLineTo (CairoT fcr) x y = withForeignPtr fcr \cr -> c_cairo_rel_line_to cr x y

foreign import ccall "cairo_move_to" c_cairo_move_to ::
	Ptr CairoT -> #{type double} -> #{type double} -> IO ()

foreign import ccall "cairo_line_to" c_cairo_line_to ::
	Ptr CairoT -> #{type double} -> #{type double} -> IO ()

foreign import ccall "cairo_rel_line_to" c_cairo_rel_line_to ::
	Ptr CairoT -> #{type double} -> #{type double} -> IO ()

cairoArc :: CairoT -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> IO ()
cairoArc (CairoT fcr) xc yc r a1 a2 =
	withForeignPtr fcr \cr -> c_cairo_arc cr xc yc r a1 a2

foreign import ccall "cairo_arc" c_cairo_arc ::
	Ptr CairoT -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> IO ()

cairoRelCurveTo :: CairoT -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} ->
	#{type double} -> #{type double} -> IO ()
cairoRelCurveTo (CairoT fcr) dx1 dy1 dx2 dy2 dx3 dy3 = withForeignPtr fcr \cr ->
	c_cairo_rel_curve_to cr dx1 dy1 dx2 dy2 dx3 dy3

foreign import ccall "cairo_rel_curve_to" c_cairo_rel_curve_to ::
	Ptr CairoT -> #{type double} -> #{type double} ->
		#{type double} -> #{type double} ->
		#{type double} -> #{type double} -> IO ()

cairoClosePath :: CairoT -> IO ()
cairoClosePath (CairoT fcr) = withForeignPtr fcr c_cairo_close_path

foreign import ccall "cairo_close_path" c_cairo_close_path ::
	Ptr CairoT -> IO ()

foreign import ccall "cairo_rectangle" c_cairo_rectangle ::
	Ptr CairoT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoRectangle :: CairoT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoRectangle (CairoT cr) x y w h = withForeignPtr cr \p -> c_cairo_rectangle p x y w h
