{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Exception
import Data.Int

import Graphics.Gdk.Types

import Graphics.Cairo.Types

#include <gdk/gdk.h>

newtype GMainLoop = GMainLoop (Ptr GMainLoop) deriving Show

data CairoRectangleIntT = CairoRectangleIntT {
	cairoRectangleIntTX, cairoRectangleIntTY :: #{type int},
	cairoRectangleIntTWidth, cairoRectangleIntTHeight :: #{type int} } deriving Show

instance Storable CairoRectangleIntT where
	sizeOf _ = #size cairo_rectangle_int_t
	alignment _ = #alignment cairo_rectangle_int_t
	peek pr = do
		x <- #{peek cairo_rectangle_int_t, x} pr
		y <- #{peek cairo_rectangle_int_t, y} pr
		w <- #{peek cairo_rectangle_int_t, width} pr
		h <- #{peek cairo_rectangle_int_t, height} pr
		pure $ CairoRectangleIntT x y w h
	poke pr (CairoRectangleIntT x y w h) = do
		#{poke cairo_rectangle_int_t, x} pr x
		#{poke cairo_rectangle_int_t, y} pr y
		#{poke cairo_rectangle_int_t, width} pr w
		#{poke cairo_rectangle_int_t, height} pr h

foreign import ccall "cairo_region_create_rectangle" c_cairo_region_create_rectangle ::
	Ptr CairoRectangleIntT -> IO (Ptr CairoRegionT)

foreign import ccall "cairo_region_destroy" c_cairo_region_destroy ::
	Ptr CairoRegionT -> IO ()

cairoRegionWithRectangle :: CairoRectangleIntT -> (CairoRegionT -> IO a) -> IO a
cairoRegionWithRectangle r = bracket
	(alloca \p -> poke p r *> c_cairo_region_create_rectangle p)
	c_cairo_region_destroy . (. CairoRegionT)

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
