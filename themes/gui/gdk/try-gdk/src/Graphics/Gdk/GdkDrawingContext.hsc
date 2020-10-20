{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDrawingContext where

import Foreign.Ptr

import Graphics.Gdk.Types

import Graphics.Cairo.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_drawing_context_get_cairo_context" c_gdk_drawing_context_get_cairo_context ::
	Ptr GdkDrawingContext -> IO (Ptr (CairoT s))

gdkDrawingContextGetCairoContext :: GdkDrawingContext -> IO (CairoT s)
gdkDrawingContextGetCairoContext (GdkDrawingContext c) = makeCairoT =<< c_gdk_drawing_context_get_cairo_context c
