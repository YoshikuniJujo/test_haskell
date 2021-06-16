{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDrawingContext where

import Foreign.Ptr
import Foreign.Concurrent
import Data.Int
import System.GLib.Bool

import Data.CairoContext
import Graphics.Gdk.Types

import Graphics.Cairo.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_drawing_context_get_window" c_gdk_drawing_context_get_window ::
	Ptr GdkDrawingContext -> IO (Ptr GdkWindow)

gdkDrawingContextGetWindow :: GdkDrawingContext -> IO GdkWindow
gdkDrawingContextGetWindow (GdkDrawingContext p) = GdkWindow <$> c_gdk_drawing_context_get_window p

foreign import ccall "gdk_drawing_context_get_clip" c_gdk_drawing_context_get_clip ::
	Ptr GdkDrawingContext -> IO (Ptr (CairoRegionT s))

gdkDrawingContextGetClip :: GdkDrawingContext -> IO (CairoRegionT s)
gdkDrawingContextGetClip (GdkDrawingContext p) = makeCairoRegionT =<< c_gdk_drawing_context_get_clip p

foreign import ccall "gdk_drawing_context_get_cairo_context" c_gdk_drawing_context_get_cairo_context ::
	Ptr GdkDrawingContext -> IO (Ptr (CairoT s))

gdkDrawingContextGetCairoContext :: GdkDrawingContext -> IO (CairoT s)
gdkDrawingContextGetCairoContext (GdkDrawingContext c) = do
	p <- c_gdk_drawing_context_get_cairo_context c
	fp <- newForeignPtr p $ pure ()
	pure $ CairoT fp

foreign import ccall "gdk_drawing_context_is_valid" c_gdk_drawing_context_is_valid ::
	Ptr GdkDrawingContext -> IO #type gboolean

gdkDrawingContextIsValid :: GdkDrawingContext -> IO Bool
gdkDrawingContextIsValid (GdkDrawingContext p) = gbooleanToBool <$> c_gdk_drawing_context_is_valid p
