{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Path where

import Foreign.Ptr
import Foreign.ForeignPtr

import Graphics.Cairo

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
