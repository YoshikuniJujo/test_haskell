{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice where

import Foreign.Ptr
import Foreign.C

import Graphics.Gdk.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_device_get_name" c_gdk_device_get_name ::
	Ptr GdkDevice -> IO CString

gdkDeviceGetName :: GdkDevice -> IO String
gdkDeviceGetName (GdkDevice p) = peekCString =<< c_gdk_device_get_name p
