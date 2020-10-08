{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Png where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Data.Word

import Graphics.Cairo

#include <cairo.h>

foreign import ccall "cairo_surface_write_to_png" c_cairo_surface_write_to_png ::
	Ptr CairoSurfaceT -> CString -> IO #type cairo_status_t

cairoSurfaceWriteToPng :: CairoSurfaceT -> FilePath -> IO CairoStatusT
cairoSurfaceWriteToPng (CairoSurfaceT s) fp = CairoStatusT
	<$> (withForeignPtr s \p -> c_cairo_surface_write_to_png p =<< newCString fp)
