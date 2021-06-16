{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.PointsAndRectangles where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Data.Int
import System.GLib.Bool

import Graphics.Gdk.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_rectangle_intersect" c_gdk_rectangle_intersect ::
	Ptr GdkRectangle -> Ptr GdkRectangle -> Ptr GdkRectangle -> IO #type gboolean

gdkRectangleIntersect :: GdkRectangle -> GdkRectangle -> IO (Maybe GdkRectangle)
gdkRectangleIntersect src1 src2 = alloca \s1 -> alloca \s2 -> alloca \d -> do
	poke s1 src1
	poke s2 src2
	c_gdk_rectangle_intersect s1 s2 d >>= \case
		#{const FALSE} -> pure Nothing
		_ -> Just <$> peek d

foreign import ccall "gdk_rectangle_union" c_gdk_rectangle_union ::
	Ptr GdkRectangle -> Ptr GdkRectangle -> Ptr GdkRectangle -> IO ()

gdkRectangleUnion :: GdkRectangle -> GdkRectangle -> IO GdkRectangle
gdkRectangleUnion src1 src2 = alloca \s1 -> alloca \s2 -> alloca \d -> do
	poke s1 src1
	poke s2 src2
	c_gdk_rectangle_union s1 s2 d
	peek d

foreign import ccall "gdk_rectangle_equal" c_gdk_rectangle_equal ::
	Ptr GdkRectangle -> Ptr GdkRectangle -> IO #type gboolean

gdkRectangleEqual :: GdkRectangle -> GdkRectangle -> IO Bool
gdkRectangleEqual rect1 rect2 = alloca \r1 -> alloca \r2 -> do
	poke r1 rect1
	poke r2 rect2
	gbooleanToBool <$> c_gdk_rectangle_equal r1 r2
