{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDrawingContext.Internal (

	-- * GDK DRAWING CONTEXT
	GdkDrawingContext(..),

	-- * IS VALID
	gdkDrawingContextIsValid,

	-- * WINDOW AND CLIP
	gdkDrawingContextGetWindow, gdkDrawingContextGetClip,

	-- * CAIRO CONTEXT
	gdkDrawingContextGetCairoContext ) where

import Foreign.Ptr
import Foreign.Concurrent
import Control.Monad.ST
import Data.Int
import System.GLib.Bool

import Data.CairoContext
import {-# SOURCE #-} Graphics.Gdk.Windows.Internal

import Graphics.Cairo.Drawing.Regions

#include <gdk/gdk.h>

newtype GdkDrawingContext s = GdkDrawingContext (Ptr (GdkDrawingContext s))
	deriving Show

foreign import ccall "gdk_drawing_context_get_window"
	gdkDrawingContextGetWindow :: GdkDrawingContext s -> IO GdkWindow

gdkDrawingContextGetClip :: GdkDrawingContext s -> IO (CairoRegionT RealWorld)
gdkDrawingContextGetClip dc =
	makeCairoRegionT =<< c_gdk_drawing_context_get_clip dc

foreign import ccall "gdk_drawing_context_get_clip"
	c_gdk_drawing_context_get_clip ::
		GdkDrawingContext s -> IO (Ptr (CairoRegionT RealWorld))

gdkDrawingContextGetCairoContext :: GdkDrawingContext s -> IO (CairoTIO s)
gdkDrawingContextGetCairoContext dc = do
	p <- c_gdk_drawing_context_get_cairo_context dc
	fp <- newForeignPtr p $ pure ()
	pure $ CairoT fp

foreign import ccall "gdk_drawing_context_get_cairo_context"
	c_gdk_drawing_context_get_cairo_context ::
		GdkDrawingContext s -> IO (Ptr (CairoTIO s))

gdkDrawingContextIsValid :: GdkDrawingContext s -> IO Bool
gdkDrawingContextIsValid dc =
	gbooleanToBool <$> c_gdk_drawing_context_is_valid dc

foreign import ccall "gdk_drawing_context_is_valid"
	c_gdk_drawing_context_is_valid ::
		GdkDrawingContext s -> IO #{type gboolean}
