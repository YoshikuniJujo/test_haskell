{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows where

import Foreign.Ptr
import Data.Bits
import Data.Word

import Graphics.Gdk.Types
import Graphics.Gdk.Values

#include <gdk/gdk.h>

mergeGdkWindowAttributesType :: [GdkWindowAttributesType] -> #{type GdkWindowAttributesType}
mergeGdkWindowAttributesType [] = 0
mergeGdkWindowAttributesType (GdkWindowAttributesType at : ats) =
	at .|. mergeGdkWindowAttributesType ats

foreign import ccall "gdk_window_new" c_gdk_window_new ::
	Ptr GdkWindow -> Ptr GdkWindowAttr -> #{type GdkWindowAttributesType} -> IO (Ptr GdkWindow)

gdkWindowNew :: Maybe GdkWindow -> GdkWindowAttr -> [GdkWindowAttributesType] -> IO GdkWindow
gdkWindowNew mp (GdkWindowAttr attr) am = GdkWindow <$> c_gdk_window_new
	(maybe nullPtr (\(GdkWindow p) -> p) mp) attr (mergeGdkWindowAttributesType am)
