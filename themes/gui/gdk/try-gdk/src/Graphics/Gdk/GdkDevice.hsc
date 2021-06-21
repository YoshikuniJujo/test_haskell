{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C
import Data.Word

import Graphics.Gdk.Types
import Graphics.Gdk.Values

import System.GLib.DoublyLinkedLists

#include <gdk/gdk.h>

foreign import ccall "gdk_device_get_name" c_gdk_device_get_name ::
	Ptr GdkDevice -> IO CString

gdkDeviceGetName :: GdkDevice -> IO String
gdkDeviceGetName (GdkDevice fp) = withForeignPtr fp \p -> peekCString =<< c_gdk_device_get_name p

foreign import ccall "gdk_device_get_vendor_id" c_gdk_device_get_vendor_id ::
	Ptr GdkDevice -> IO CString

gdkDeviceGetVendorId :: GdkDevice -> IO (Maybe String)
gdkDeviceGetVendorId (GdkDevice fp) = withForeignPtr fp \p -> c_gdk_device_get_vendor_id p >>= \case
	cs	| cs == nullPtr -> pure Nothing
		| otherwise -> Just <$> peekCString cs

foreign import ccall "gdk_device_get_product_id" c_gdk_device_get_product_id ::
	Ptr GdkDevice -> IO CString

gdkDeviceGetProductId :: GdkDevice -> IO (Maybe String)
gdkDeviceGetProductId (GdkDevice fp) = withForeignPtr fp \p -> c_gdk_device_get_product_id p >>= \case
	cs	| cs == nullPtr -> pure Nothing
		| otherwise -> Just <$> peekCString cs

foreign import ccall "gdk_device_get_source" c_gdk_device_get_source ::
	Ptr GdkDevice -> IO #type GdkInputSource

gdkDeviceGetSource :: GdkDevice -> IO GdkInputSource
gdkDeviceGetSource (GdkDevice fp) = withForeignPtr fp \p -> GdkInputSource <$> c_gdk_device_get_source p

foreign import ccall "gdk_device_list_slave_devices" c_gdk_device_list_slave_devices ::
	Ptr GdkDevice -> IO (Ptr (GList GdkDevice))

mkGdkDevice :: Ptr GdkDevice -> IO GdkDevice
mkGdkDevice p = GdkDevice <$> newForeignPtr p (pure ())

gdkDeviceListSlaveDevices :: GdkDevice -> IO ([GdkDevice], [GdkDevice])
gdkDeviceListSlaveDevices (GdkDevice fp) = withForeignPtr fp \p -> do
	gl <- c_gdk_device_list_slave_devices p
	(\(x, y) -> (,) <$> mapM mkGdkDevice x <*> mapM mkGdkDevice y) =<< gListListPtr (GListRef gl)
		<* c_g_list_free gl

foreign import ccall "g_list_free" c_g_list_free ::
	Ptr (GList a) -> IO ()

{-
foreign import ccall "gdk_device_get_device_tool" c_gdk_device_get_device_tool ::
	Ptr GdkDevice -> IO (Ptr GdkDeviceTool)
	-}

foreign import ccall "gdk_device_tool_get_tool_type" c_gdk_device_get_tool_type ::
	Ptr GdkDeviceTool -> IO #type GdkDeviceToolType

gdkDeviceToolGetToolType :: GdkDeviceTool -> IO GdkDeviceToolType
gdkDeviceToolGetToolType (GdkDeviceTool fdt) = withForeignPtr fdt \dt ->
	GdkDeviceToolType <$> c_gdk_device_get_tool_type dt
