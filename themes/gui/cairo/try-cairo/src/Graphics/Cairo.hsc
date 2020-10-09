{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo where

#include <cairo.h>

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Data.Word
import Data.Int

newtype CairoT = CairoT (ForeignPtr CairoT) deriving Show

foreign import ccall "cairo_create" c_cairo_create :: Ptr CairoSurfaceT -> IO (Ptr CairoT)

foreign import ccall "cairo_destroy" c_cairo_destroy :: Ptr CairoT -> IO ()

makeCairoT :: Ptr CairoT -> IO CairoT
makeCairoT p = CairoT <$> newForeignPtr p (c_cairo_destroy p)

cairoCreate :: CairoSurfaceT -> IO CairoT
cairoCreate (CairoSurfaceT s) = makeCairoT =<< withForeignPtr s \p -> c_cairo_create p

newtype CairoSurfaceT = CairoSurfaceT (ForeignPtr CairoSurfaceT) deriving Show

newtype CairoStatusT = CairoStatusT #{type cairo_status_t} deriving Show

#enum CairoStatusT, CairoStatusT, CAIRO_STATUS_SUCCESS, \
	CAIRO_STATUS_NO_MEMORY, CAIRO_STATUS_INVALID_RESTORE

foreign import ccall "cairo_image_surface_create" c_cairo_image_surface_create ::
	#{type cairo_format_t} -> #{type int} -> #{type int} -> IO (Ptr CairoSurfaceT)

foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy ::
	Ptr CairoSurfaceT -> IO ()

makeCairoSurfaceT :: Ptr CairoSurfaceT -> IO CairoSurfaceT
makeCairoSurfaceT p = CairoSurfaceT <$> newForeignPtr p (c_cairo_surface_destroy p)

cairoImageSurfaceCreate :: CairoFormatT -> #{type int} -> #{type int} -> IO CairoSurfaceT
cairoImageSurfaceCreate (CairoFormatT f) w h =
	makeCairoSurfaceT =<< c_cairo_image_surface_create f w h

newtype CairoFormatT = CairoFormatT #{type cairo_format_t} deriving Show

#enum CairoFormatT, CairoFormatT, CAIRO_FORMAT_INVALID, CAIRO_FORMAT_ARGB32

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

foreign import ccall "cairo_rectangle" c_cairo_rectangle ::
	Ptr CairoT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoRectangle :: CairoT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoRectangle (CairoT cr) x y w h = withForeignPtr cr \p -> c_cairo_rectangle p x y w h

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
