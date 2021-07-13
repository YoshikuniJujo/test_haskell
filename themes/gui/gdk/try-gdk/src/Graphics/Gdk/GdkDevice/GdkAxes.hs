{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice.GdkAxes where

import Foreign.ForeignPtr
import Foreign.C.Types

import Graphics.Gdk.GdkDevice

newtype Axes = Axes (ForeignPtr CDouble) deriving Show

foreign import ccall "gdk_device_get_n_axes"
	gdkDeviceGetNAxes :: GdkDevice -> IO CInt
