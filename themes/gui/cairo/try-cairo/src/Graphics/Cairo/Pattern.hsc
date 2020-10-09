{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Pattern where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

import Graphics.Cairo

#include <cairo.h>

newtype CairoPatternT = CairoPatternT (ForeignPtr CairoPatternT) deriving Show

makeCairoPatternT :: Ptr CairoPatternT -> IO CairoPatternT
makeCairoPatternT p = CairoPatternT <$> newForeignPtr p (c_cairo_pattern_destroy p)

foreign import ccall "cairo_pattern_destroy" c_cairo_pattern_destroy ::
	Ptr CairoPatternT -> IO ()

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

foreign import ccall "cairo_set_source" c_cairo_set_source ::
	Ptr CairoT -> Ptr CairoPatternT -> IO ()

cairoSetSource :: CairoT -> CairoPatternT -> IO ()
cairoSetSource (CairoT fcr) (CairoPatternT fpt) =
	withForeignPtr fcr \cr -> withForeignPtr fpt \pt ->
		c_cairo_set_source cr pt

foreign import ccall "cairo_mask" c_cairo_mask ::
	Ptr CairoT -> Ptr CairoPatternT -> IO ()

cairoMask :: CairoT -> CairoPatternT -> IO ()
cairoMask (CairoT fcr) (CairoPatternT fpt) =
	withForeignPtr fcr \cr -> withForeignPtr fpt \pt ->
		c_cairo_mask cr pt

foreign import ccall "cairo_push_group" c_cairo_push_group ::
	Ptr CairoT -> IO ()

cairoPushGroup :: CairoT -> IO ()
cairoPushGroup (CairoT fcr) = withForeignPtr fcr c_cairo_push_group

foreign import ccall "cairo_pop_group_to_source" c_cairo_pop_group_to_source ::
	Ptr CairoT -> IO ()

cairoPopGroupToSource :: CairoT -> IO ()
cairoPopGroupToSource (CairoT fcr) = withForeignPtr fcr c_cairo_pop_group_to_source

foreign import ccall "cairo_pop_group" c_cairo_pop_group ::
	Ptr CairoT -> IO (Ptr CairoPatternT)

cairoPopGroup :: CairoT -> IO CairoPatternT
cairoPopGroup (CairoT fcr) = makeCairoPatternT =<< withForeignPtr fcr c_cairo_pop_group
