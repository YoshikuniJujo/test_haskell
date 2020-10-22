{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDisplay where

import Foreign.Ptr
import Foreign.C

import Graphics.Gdk.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_display_get_name" c_gdk_display_get_name ::
	Ptr GdkDisplay -> IO CString

gdkDisplayGetName :: GdkDisplay -> IO String
gdkDisplayGetName (GdkDisplay p) = peekCString =<< c_gdk_display_get_name p
