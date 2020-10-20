{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk where

import Foreign.Ptr
import Foreign.Storable
import Data.Int

import Graphics.Gdk.Types

import Graphics.Cairo.Types

#include <gdk/gdk.h>

newtype GMainLoop = GMainLoop (Ptr GMainLoop) deriving Show

foreign import ccall "gdk_drawing_context_get_cairo_context" c_gdk_drawing_context_get_cairo_context ::
	Ptr GdkDrawingContext -> IO (Ptr (CairoT s))

gdkDrawingContextGetCairoContext :: GdkDrawingContext -> IO (CairoT s)
gdkDrawingContextGetCairoContext (GdkDrawingContext c) = makeCairoT =<< c_gdk_drawing_context_get_cairo_context c

gdkRectangleSetX, gdkRectangleSetY :: GdkRectangle -> #{type int} -> IO ()
gdkRectangleSetX (GdkRectangle p) = #{poke GdkRectangle, x} p
gdkRectangleSetY (GdkRectangle p) = #{poke GdkRectangle, y} p

gdkRectangleSetWidth, gdkRectangleSetHeight :: GdkRectangle -> #{type int} -> IO ()
gdkRectangleSetWidth (GdkRectangle p) = #{poke GdkRectangle, width} p
gdkRectangleSetHeight (GdkRectangle p) = #{poke GdkRectangle, height} p
