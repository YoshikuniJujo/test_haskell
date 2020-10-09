{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.PngSupport (
	cairoSurfaceCreateFromPng, cairoSurfaceWriteToPng
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Data.Word

import Graphics.Cairo
import Graphics.Cairo.Types

#include <cairo.h>

foreign import ccall "cairo_surface_write_to_png" c_cairo_surface_write_to_png ::
	Ptr CairoSurfaceT -> CString -> IO #type cairo_status_t

cairoSurfaceWriteToPng :: CairoSurfaceT -> FilePath -> IO CairoStatusT
cairoSurfaceWriteToPng (CairoSurfaceT s) fp = withCString fp \cs -> CairoStatusT
	<$> (withForeignPtr s \p -> c_cairo_surface_write_to_png p cs)

foreign import ccall "cairo_image_surface_create_from_png" c_cairo_surface_create_from_png ::
	CString -> IO (Ptr CairoSurfaceT)

cairoSurfaceCreateFromPng :: FilePath -> IO CairoSurfaceT
cairoSurfaceCreateFromPng fp = withCString fp \cs ->
	makeCairoSurfaceT =<< c_cairo_surface_create_from_png cs
