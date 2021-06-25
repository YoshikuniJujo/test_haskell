{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.PointsAndRectangles where

import Control.Monad.Primitive
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Int
import System.GLib.Bool

import Graphics.Gdk.Types

#include <gdk/gdk.h>

gdkRectangleIntersect :: PrimMonad m =>
	GdkRectangle -> GdkRectangle -> GdkRectanglePrim (PrimState m) -> m Bool
gdkRectangleIntersect
	(GdkRectangle_ fs) (GdkRectangle_ ft) (GdkRectanglePrim fd) =
	unsafeIOToPrim $ gbooleanToBool
		<$> withForeignPtr fs \ps -> withForeignPtr ft \pt ->
			withForeignPtr fd $ c_gdk_rectangle_intersect ps pt

foreign import ccall "gdk_rectangle_intersect" c_gdk_rectangle_intersect ::
	Ptr GdkRectangle -> Ptr GdkRectangle -> Ptr GdkRectangle -> IO #type gboolean

gdkRectangleUnion :: PrimMonad m =>
	GdkRectangle -> GdkRectangle -> GdkRectanglePrim (PrimState m) -> m ()
gdkRectangleUnion (GdkRectangle_ fs) (GdkRectangle_ ft) (GdkRectanglePrim fd) =
	unsafeIOToPrim $ withForeignPtr fs \ps -> withForeignPtr ft \pt ->
		withForeignPtr fd $ c_gdk_rectangle_union ps pt

foreign import ccall "gdk_rectangle_union" c_gdk_rectangle_union ::
	Ptr GdkRectangle -> Ptr GdkRectangle -> Ptr GdkRectangle -> IO ()

gdkRectangleEqual :: GdkRectangle -> GdkRectangle -> IO Bool
gdkRectangleEqual (GdkRectangle_ fr1) (GdkRectangle_ fr2) =
	withForeignPtr fr1 \pr1 -> withForeignPtr fr2 \pr2 ->
		gbooleanToBool <$> c_gdk_rectangle_equal pr1 pr2

foreign import ccall "gdk_rectangle_equal" c_gdk_rectangle_equal ::
	Ptr GdkRectangle -> Ptr GdkRectangle -> IO #type gboolean
