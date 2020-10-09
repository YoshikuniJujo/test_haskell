{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.CairoT (
	-- * Basic
	cairoCreate,
	-- * Push and Pop Group
	cairoPushGroup, cairoPopGroup, cairoPopGroupToSource,
	-- * Set Source
	cairoSetSourceRgb, cairoSetSourceRgba, cairoSetSource,
	-- * Set Attribute
	cairoSetLineWidth,
	-- * Verb
	cairoFill, cairoMask, cairoPaint, cairoPaintWithAlpha, cairoStroke,
	) where

#include <cairo.h>

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)

import Graphics.Cairo.Types

foreign import ccall "cairo_create" c_cairo_create :: Ptr CairoSurfaceT -> IO (Ptr CairoT)

cairoCreate :: CairoSurfaceT -> IO CairoT
cairoCreate (CairoSurfaceT s) = makeCairoT =<< withForeignPtr s \p -> c_cairo_create p

foreign import ccall "cairo_set_line_width" c_cairo_set_line_width ::
	Ptr CairoT -> #{type double} -> IO ()

cairoSetLineWidth :: CairoT -> #{type double} -> IO ()
cairoSetLineWidth (CairoT cr) w = withForeignPtr cr \p -> c_cairo_set_line_width p w

foreign import ccall "cairo_set_source_rgb" c_cairo_set_source_rgb ::
	Ptr CairoT -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoSetSourceRgb :: CairoT -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoSetSourceRgb (CairoT cr) r g b = withForeignPtr cr \p -> c_cairo_set_source_rgb p r g b

foreign import ccall "cairo_set_source_rgba" c_cairo_set_source_rgba ::
	Ptr CairoT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoSetSourceRgba :: CairoT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoSetSourceRgba (CairoT cr) r g b a = withForeignPtr cr \p -> c_cairo_set_source_rgba p r g b a

foreign import ccall "cairo_stroke" c_cairo_stroke :: Ptr CairoT -> IO ()

cairoStroke :: CairoT -> IO ()
cairoStroke (CairoT cr) = withForeignPtr cr c_cairo_stroke

foreign import ccall "cairo_fill" c_cairo_fill :: Ptr CairoT -> IO ()

cairoFill :: CairoT -> IO ()
cairoFill (CairoT cr) = withForeignPtr cr c_cairo_fill

foreign import ccall "cairo_paint" c_cairo_paint ::
	Ptr CairoT -> IO ()

cairoPaint :: CairoT -> IO ()
cairoPaint (CairoT fcr) = withForeignPtr fcr c_cairo_paint

foreign import ccall "cairo_paint_with_alpha" c_cairo_paint_with_alpha ::
	Ptr CairoT -> #{type double} -> IO ()

cairoPaintWithAlpha :: CairoT -> #{type double} -> IO ()
cairoPaintWithAlpha (CairoT fcr) a = withForeignPtr fcr \cr -> c_cairo_paint_with_alpha cr a

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
