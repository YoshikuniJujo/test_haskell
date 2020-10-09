{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo (
	CairoT, CairoSurfaceT, CairoStatusT,
	cairoFill, cairoRectangle, cairoSetSourceRgba, cairoStroke, cairoSetLineWidth,
	cairoSetSourceRgb, cairoCreate, cairoFormatArgb32, cairoImageSurfaceCreate,
	cairoPaintWithAlpha, cairoPaint,

	cairoStatusSuccess, cairoStatusNoMemory, cairoStatusInvalidRestore, cairoFormatInvalid
	) where

#include <cairo.h>

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Data.Int

import Graphics.Cairo.CairoT
import Graphics.Cairo.Types

foreign import ccall "cairo_image_surface_create" c_cairo_image_surface_create ::
	#{type cairo_format_t} -> #{type int} -> #{type int} -> IO (Ptr CairoSurfaceT)

cairoImageSurfaceCreate :: CairoFormatT -> #{type int} -> #{type int} -> IO CairoSurfaceT
cairoImageSurfaceCreate (CairoFormatT f) w h =
	makeCairoSurfaceT =<< c_cairo_image_surface_create f w h

newtype CairoFormatT = CairoFormatT #{type cairo_format_t} deriving Show

#enum CairoFormatT, CairoFormatT, CAIRO_FORMAT_INVALID, CAIRO_FORMAT_ARGB32

foreign import ccall "cairo_rectangle" c_cairo_rectangle ::
	Ptr CairoT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoRectangle :: CairoT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoRectangle (CairoT cr) x y w h = withForeignPtr cr \p -> c_cairo_rectangle p x y w h
