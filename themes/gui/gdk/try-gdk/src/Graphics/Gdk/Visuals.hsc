{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Visuals where

import Foreign.Ptr
import Data.Int
import Data.Word

import Graphics.Gdk.Types
import Graphics.Gdk.Values

#include <gdk/gdk.h>

foreign import ccall "gdk_visual_get_depth" c_gdk_visual_get_depth ::
	Ptr GdkVisual -> IO #type gint

gdkVisualGetDepth :: GdkVisual -> IO #type gint
gdkVisualGetDepth (GdkVisual p) = c_gdk_visual_get_depth p

foreign import ccall "gdk_visual_get_visual_type" c_gdk_visual_get_visual_type ::
	Ptr GdkVisual -> IO #type GdkVisualType

gdkVisualGetVisualType :: GdkVisual -> IO GdkVisualType
gdkVisualGetVisualType (GdkVisual p) = GdkVisualType <$> c_gdk_visual_get_visual_type p
