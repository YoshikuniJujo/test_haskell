{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDrawingContext (
	GdkDrawingContext(..),
	gdkDrawingContextGetWindow, gdkDrawingContextGetClip,
	gdkDrawingContextGetCairoContext, gdkDrawingContextIsValid ) where

import Foreign.Ptr
import Foreign.Concurrent
import Data.Int
import System.GLib.Bool

import Data.CairoContext
import {-# SOURCE #-} Graphics.Gdk.Windows

import Graphics.Cairo.Drawing.Regions

#include <gdk/gdk.h>

newtype GdkDrawingContext s = GdkDrawingContext (Ptr (GdkDrawingContext s)) deriving Show

foreign import ccall "gdk_drawing_context_get_window" c_gdk_drawing_context_get_window ::
	Ptr (GdkDrawingContext s) -> IO (Ptr GdkWindow)

gdkDrawingContextGetWindow :: GdkDrawingContext s -> IO GdkWindow
gdkDrawingContextGetWindow (GdkDrawingContext p) = GdkWindow <$> c_gdk_drawing_context_get_window p

foreign import ccall "gdk_drawing_context_get_clip" c_gdk_drawing_context_get_clip ::
	Ptr (GdkDrawingContext s) -> IO (Ptr (CairoRegionT s))

gdkDrawingContextGetClip :: GdkDrawingContext s -> IO (CairoRegionT s)
gdkDrawingContextGetClip (GdkDrawingContext p) = makeCairoRegionT =<< c_gdk_drawing_context_get_clip p

foreign import ccall "gdk_drawing_context_get_cairo_context" c_gdk_drawing_context_get_cairo_context ::
	Ptr (GdkDrawingContext s) -> IO (Ptr (CairoT s'))

gdkDrawingContextGetCairoContext :: GdkDrawingContext s -> IO (CairoT s')
gdkDrawingContextGetCairoContext (GdkDrawingContext c) = do
	p <- c_gdk_drawing_context_get_cairo_context c
	fp <- newForeignPtr p $ pure ()
	pure $ CairoT fp

foreign import ccall "gdk_drawing_context_is_valid" c_gdk_drawing_context_is_valid ::
	Ptr (GdkDrawingContext s) -> IO #type gboolean

gdkDrawingContextIsValid :: GdkDrawingContext s -> IO Bool
gdkDrawingContextIsValid (GdkDrawingContext p) = gbooleanToBool <$> c_gdk_drawing_context_is_valid p
