{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice.GdkAxes where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import System.GLib.DoublyLinkedLists

import Graphics.Gdk.GdkDevice
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

newtype Axes = Axes (ForeignPtr CDouble) deriving Show

foreign import ccall "gdk_device_get_n_axes"
	gdkDeviceGetNAxes :: GdkDevice -> IO CInt

gdkDeviceListAxes :: GdkDevice -> IO [GdkAtom]
gdkDeviceListAxes d =
	map GdkAtom <$> (g_list_to_list =<< c_gdk_device_list_axes d)

foreign import ccall "gdk_device_list_axes"
	c_gdk_device_list_axes :: GdkDevice -> IO (Ptr (GList GdkAtom))
