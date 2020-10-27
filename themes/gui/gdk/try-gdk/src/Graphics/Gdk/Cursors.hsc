{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Cursors where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C

import Graphics.Gdk.Types
import Graphics.Cairo.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_cursor_new_from_surface" c_gdk_cursor_new_from_surface ::
	Ptr GdkDisplay -> Ptr (CairoSurfaceT s) -> #{type gdouble} -> #{type gdouble} -> IO (Ptr GdkCursor)

gdkCursorNewFromSurface :: GdkDisplay -> CairoSurfaceT s -> #{type gdouble} -> #{type gdouble} -> IO GdkCursor
gdkCursorNewFromSurface (GdkDisplay d) (CairoSurfaceT fs) x y = withForeignPtr fs \s ->
	mkGdkCursor =<< c_gdk_cursor_new_from_surface d s x y

foreign import ccall "gdk_cursor_new_from_name" c_gdk_cursor_new_from_name ::
	Ptr GdkDisplay -> CString -> IO (Ptr GdkCursor)

gdkCursorNewFromName :: GdkDisplay -> String -> IO GdkCursor
gdkCursorNewFromName (GdkDisplay d) nm = withCString nm \cnm ->
	mkGdkCursor =<< c_gdk_cursor_new_from_name d cnm
