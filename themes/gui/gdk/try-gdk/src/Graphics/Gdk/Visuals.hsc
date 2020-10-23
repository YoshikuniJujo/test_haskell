{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Visuals where

import Foreign.Ptr
import Data.Int

import Graphics.Gdk.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_visual_get_depth" c_gdk_visual_get_depth ::
	Ptr GdkVisual -> IO #type gint

gdkVisualGetDepth :: GdkVisual -> IO #type gint
gdkVisualGetDepth (GdkVisual p) = c_gdk_visual_get_depth p
