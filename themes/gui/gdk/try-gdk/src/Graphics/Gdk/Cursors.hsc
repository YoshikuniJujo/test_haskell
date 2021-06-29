{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Cursors (
	gdkCursorNewFromSurface,
	gdkCursorNewFromName,
	gdkCursorGetDisplay,
	gdkCursorGetSurface,
	gdkCursorGetCursorType ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Data.Int

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Types
import Graphics.Gdk.Values

import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal

#include <gdk/gdk.h>

foreign import ccall "gdk_cursor_new_from_surface" c_gdk_cursor_new_from_surface ::
	Ptr GdkDisplay -> Ptr (CairoSurfaceT s ps) -> #{type gdouble} -> #{type gdouble} -> IO (Ptr GdkCursor)

gdkCursorNewFromSurface :: GdkDisplay -> CairoSurfaceT s ps -> #{type gdouble} -> #{type gdouble} -> IO GdkCursor
gdkCursorNewFromSurface (GdkDisplay d) (CairoSurfaceT fs) x y = withForeignPtr fs \s ->
	mkGdkCursor =<< c_gdk_cursor_new_from_surface d s x y

foreign import ccall "gdk_cursor_new_from_name" c_gdk_cursor_new_from_name ::
	Ptr GdkDisplay -> CString -> IO (Ptr GdkCursor)

gdkCursorNewFromName :: GdkDisplay -> String -> IO GdkCursor
gdkCursorNewFromName (GdkDisplay d) nm = withCString nm \cnm ->
	mkGdkCursor =<< c_gdk_cursor_new_from_name d cnm

foreign import ccall "gdk_cursor_get_display" c_gdk_cursor_get_display ::
	Ptr GdkCursor -> IO (Ptr GdkDisplay)

gdkCursorGetDisplay :: GdkCursor -> IO GdkDisplay
gdkCursorGetDisplay (GdkCursor fc) = withForeignPtr fc \c ->
	GdkDisplay <$> c_gdk_cursor_get_display c

foreign import ccall "gdk_cursor_get_surface" c_gdk_cursor_get_surface ::
	Ptr GdkCursor -> IO (Ptr (CairoSurfaceT s ps))

gdkCursorGetSurface :: GdkCursor -> IO (CairoSurfaceT s ps)
gdkCursorGetSurface (GdkCursor fc) = withForeignPtr fc \c ->
	mkCairoSurfaceT =<< c_gdk_cursor_get_surface c

foreign import ccall "gdk_cursor_get_cursor_type" c_gdk_cursor_get_cursor_type ::
	Ptr GdkCursor -> IO #{type GdkCursorType}

gdkCursorGetCursorType :: GdkCursor -> IO GdkCursorType
gdkCursorGetCursorType (GdkCursor fc) = withForeignPtr fc \c ->
	GdkCursorType <$> c_gdk_cursor_get_cursor_type c
