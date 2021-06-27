{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice (
	-- * FUNCTION
	gdkDeviceGetName,
	gdkDeviceGetVendorId,
	gdkDeviceGetProductId,
	gdkDeviceGetSource,
	gdkDeviceListSlaveDevices,
	gdkDeviceToolGetToolType,

	-- * GDK INPUT SOURCE
	GdkInputSource(..),
	pattern GdkSourceMouse, pattern GdkSourcePen, pattern GdkSourceEraser,
	pattern GdkSourceCursor, pattern GdkSourceKeyboard,
	pattern GdkSourceTouchscreen, pattern GdkSourceTouchpad,
	pattern GdkSourceTrackpoint, pattern GdkSourceTabletPad
	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C
import Foreign.C.Enum
import Data.Word

import Graphics.Gdk.Types
import Graphics.Gdk.Values

import System.GLib.DoublyLinkedLists

#include <gdk/gdk.h>

gdkDeviceGetName :: GdkDevice -> IO String
gdkDeviceGetName (GdkDevice fp) =
	withForeignPtr fp \p -> peekCString =<< c_gdk_device_get_name p

foreign import ccall "gdk_device_get_name" c_gdk_device_get_name ::
	Ptr GdkDevice -> IO CString

gdkDeviceGetVendorId :: GdkDevice -> IO (Maybe String)
gdkDeviceGetVendorId (GdkDevice fp) =
	withForeignPtr fp \p -> c_gdk_device_get_vendor_id p >>= \case
		NullPtr -> pure Nothing; cs -> Just <$> peekCString cs

foreign import ccall "gdk_device_get_vendor_id" c_gdk_device_get_vendor_id ::
	Ptr GdkDevice -> IO CString

gdkDeviceGetProductId :: GdkDevice -> IO (Maybe String)
gdkDeviceGetProductId (GdkDevice fp) =
	withForeignPtr fp \p -> c_gdk_device_get_product_id p >>= \case
		NullPtr -> pure Nothing; cs -> Just <$> peekCString cs

foreign import ccall "gdk_device_get_product_id" c_gdk_device_get_product_id ::
	Ptr GdkDevice -> IO CString

enum "GdkInputSource" ''#{type GdkInputSource} [''Show, ''Eq] [
	("GdkSourceMouse", #{const GDK_SOURCE_MOUSE}),
	("GdkSourcePen", #{const GDK_SOURCE_PEN}),
	("GdkSourceEraser", #{const GDK_SOURCE_ERASER}),
	("GdkSourceCursor", #{const GDK_SOURCE_CURSOR}),
	("GdkSourceKeyboard", #{const GDK_SOURCE_KEYBOARD}),
	("GdkSourceTouchscreen", #{const GDK_SOURCE_TOUCHSCREEN}),
	("GdkSourceTouchpad", #{const GDK_SOURCE_TOUCHPAD}),
	("GdkSourceTrackpoint", #{const GDK_SOURCE_TRACKPOINT}),
	("GdkSourceTabletPad", #{const GDK_SOURCE_TABLET_PAD}) ]

gdkDeviceGetSource :: GdkDevice -> IO GdkInputSource
gdkDeviceGetSource (GdkDevice fp) = withForeignPtr fp \p -> GdkInputSource <$> c_gdk_device_get_source p

foreign import ccall "gdk_device_get_source" c_gdk_device_get_source ::
	Ptr GdkDevice -> IO #type GdkInputSource

foreign import ccall "gdk_device_list_slave_devices" c_gdk_device_list_slave_devices ::
	Ptr GdkDevice -> IO (Ptr (GList GdkDevice))

mkGdkDevice :: Ptr GdkDevice -> IO GdkDevice
mkGdkDevice p = GdkDevice <$> newForeignPtr p (pure ())

gdkDeviceListSlaveDevices :: GdkDevice -> IO [GdkDevice]
gdkDeviceListSlaveDevices (GdkDevice fp) = withForeignPtr fp \p -> do
	gl <- c_gdk_device_list_slave_devices p
	mapM mkGdkDevice =<< g_list_to_list gl <* c_g_list_free gl

{-
foreign import ccall "gdk_device_get_device_tool" c_gdk_device_get_device_tool ::
	Ptr GdkDevice -> IO (Ptr GdkDeviceTool)
	-}

foreign import ccall "gdk_device_tool_get_tool_type" c_gdk_device_get_tool_type ::
	Ptr GdkDeviceTool -> IO #type GdkDeviceToolType

gdkDeviceToolGetToolType :: GdkDeviceTool -> IO GdkDeviceToolType
gdkDeviceToolGetToolType (GdkDeviceTool fdt) = withForeignPtr fdt \dt ->
	GdkDeviceToolType <$> c_gdk_device_get_tool_type dt
