{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice (
	-- * TYPE
	GdkDevice(..),
	GdkDeviceTool(..),

	-- * FUNCTION
	gdkDeviceGetName,
	gdkDeviceGetVendorId,
	gdkDeviceGetProductId,
	gdkDeviceGetSource,
	gdkDeviceListSlaveDevices,
	gdkDeviceGetDeviceType,
	gdkDeviceGetDisplay,
	gdkDeviceGetHasCursor,
	gdkDeviceWarp,
	gdkDeviceGetSeat,
	gdkDeviceGetPosition,
	gdkDeviceGetPositionDouble,
	gdkDeviceGetWindowAtPosition,
	gdkDeviceGetWindowAtPositionDouble,
	gdkDeviceGetLastEventWindow,
	gdkDeviceToolGetToolType,

	-- * GDK INPUT SOURCE
	GdkInputSource(..),
	pattern GdkSourceMouse, pattern GdkSourcePen, pattern GdkSourceEraser,
	pattern GdkSourceCursor, pattern GdkSourceKeyboard,
	pattern GdkSourceTouchscreen, pattern GdkSourceTouchpad,
	pattern GdkSourceTrackpoint, pattern GdkSourceTabletPad,

	-- * GDK DEVICE TOOL TYPE
	GdkDeviceToolType(..),
	pattern GdkDeviceToolTypeUnknown,
	pattern GdkDeviceToolTypePen,
	pattern GdkDeviceToolTypeEraser,
	pattern GdkDeviceToolTypeBrush,
	pattern GdkDeviceToolTypePencil,
	pattern GdkDeviceToolTypeAirbrush,
	pattern GdkDeviceToolTypeMouse,
	pattern GdkDeviceToolTypeLens

	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Foreign.C.Enum
import Data.Word
import Data.Int
import System.GLib.DoublyLinkedLists
import System.GLib.Bool

import {-# SOURCE #-} Graphics.Gdk.GdkDisplay.Internal
import {-# SOURCE #-} Graphics.Gdk.GdkScreen
import {-# SOURCE #-} Graphics.Gdk.GdkSeat
import {-# SOURCE #-} Graphics.Gdk.Windows

#include <gdk/gdk.h>

newtype GdkDevice = GdkDevice (Ptr GdkDevice) deriving (Show, Storable)

gdkDeviceGetName :: GdkDevice -> IO String
gdkDeviceGetName d = peekCString =<< c_gdk_device_get_name d

foreign import ccall "gdk_device_get_name" c_gdk_device_get_name ::
	GdkDevice -> IO CString

gdkDeviceGetVendorId :: GdkDevice -> IO (Maybe String)
gdkDeviceGetVendorId d = c_gdk_device_get_vendor_id d >>= \case
	NullPtr -> pure Nothing; cs -> Just <$> peekCString cs

foreign import ccall "gdk_device_get_vendor_id" c_gdk_device_get_vendor_id ::
	GdkDevice -> IO CString

gdkDeviceGetProductId :: GdkDevice -> IO (Maybe String)
gdkDeviceGetProductId d = c_gdk_device_get_product_id d >>= \case
	NullPtr -> pure Nothing; cs -> Just <$> peekCString cs

foreign import ccall "gdk_device_get_product_id" c_gdk_device_get_product_id ::
	GdkDevice -> IO CString

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
gdkDeviceGetSource d = GdkInputSource <$> c_gdk_device_get_source d

foreign import ccall "gdk_device_get_source" c_gdk_device_get_source ::
	GdkDevice -> IO #type GdkInputSource

gdkDeviceListSlaveDevices :: GdkDevice -> IO [GdkDevice]
gdkDeviceListSlaveDevices d = do
	gl <- c_gdk_device_list_slave_devices d
	map GdkDevice <$> (g_list_to_list gl <* c_g_list_free gl)

foreign import ccall "gdk_device_list_slave_devices" c_gdk_device_list_slave_devices ::
	GdkDevice -> IO (Ptr (GList GdkDevice))

enum "GdkDeviceType" ''#{type GdkDeviceType} [''Show] [
	("GdkDeviceTypeMaster", #{const GDK_DEVICE_TYPE_MASTER}),
	("GdkDeviceTypeSlave", #{const GDK_DEVICE_TYPE_SLAVE}),
	("GdkDeviceTypeFloating", #{const GDK_DEVICE_TYPE_FLOATING}) ]

gdkDeviceGetDeviceType :: GdkDevice -> IO GdkDeviceType
gdkDeviceGetDeviceType d = GdkDeviceType <$> c_gdk_device_get_device_type d

foreign import ccall "gdk_device_get_device_type" c_gdk_device_get_device_type ::
	GdkDevice -> IO #{type GdkDeviceType}

foreign import ccall "gdk_device_get_display" gdkDeviceGetDisplay ::
	GdkDevice -> IO GdkDisplay

gdkDeviceGetHasCursor :: GdkDevice -> IO Bool
gdkDeviceGetHasCursor d = gbooleanToBool <$> c_gdk_device_get_has_cursor d

foreign import ccall "gdk_device_get_has_cursor" c_gdk_device_get_has_cursor ::
	GdkDevice -> IO #{type gboolean}

foreign import ccall "gdk_device_warp" gdkDeviceWarp ::
	GdkDevice -> GdkScreen -> CInt -> CInt -> IO ()

foreign import ccall "gdk_device_get_seat" gdkDeviceGetSeat ::
	GdkDevice -> IO GdkSeat

gdkDeviceGetPosition :: GdkDevice -> IO (GdkScreen, (CInt, CInt))
gdkDeviceGetPosition d = alloca \pps -> alloca \px -> alloca \py -> do
	c_gdk_device_get_position d pps px py
	(,) <$> (GdkScreen <$> peek pps) <*> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_position" c_gdk_device_get_position ::
	GdkDevice -> Ptr (Ptr GdkScreen) -> Ptr CInt -> Ptr CInt -> IO ()

gdkDeviceGetPositionDouble :: GdkDevice -> IO (GdkScreen, (CDouble, CDouble))
gdkDeviceGetPositionDouble d = alloca \pps -> alloca \px -> alloca \py -> do
	c_gdk_device_get_position_double d pps px py
	(,) <$> (GdkScreen <$> peek pps) <*> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_position_double"
	c_gdk_device_get_position_double ::
	GdkDevice -> Ptr (Ptr GdkScreen) -> Ptr CDouble -> Ptr CDouble -> IO ()

gdkDeviceGetWindowAtPosition :: GdkDevice -> IO (CInt, CInt)
gdkDeviceGetWindowAtPosition d = alloca \px -> alloca \py -> do
	c_gdk_device_get_window_at_position d px py
	(,) <$> peek px <*> peek py

foreign import ccall "gdk_device_get_window_at_position"
	c_gdk_device_get_window_at_position ::
	GdkDevice -> Ptr CInt -> Ptr CInt -> IO ()

gdkDeviceGetWindowAtPositionDouble :: GdkDevice -> IO (CDouble, CDouble)
gdkDeviceGetWindowAtPositionDouble d = alloca \px -> alloca \py -> do
	c_gdk_device_get_window_at_position_double d px py
	(,) <$> peek px <*> peek py

foreign import ccall "gdk_device_get_window_at_position_double"
	c_gdk_device_get_window_at_position_double ::
	GdkDevice -> Ptr CDouble -> Ptr CDouble -> IO ()

foreign import ccall "gdk_device_get_last_event_window"
	gdkDeviceGetLastEventWindow :: GdkDevice -> IO GdkWindow

newtype GdkDeviceTool = GdkDeviceTool (Ptr GdkDeviceTool) deriving Show

enum "GdkDeviceToolType" ''#{type GdkDeviceToolType} [''Show] [
	("GdkDeviceToolTypeUnknown", #{const GDK_DEVICE_TOOL_TYPE_UNKNOWN}),
	("GdkDeviceToolTypePen", #{const GDK_DEVICE_TOOL_TYPE_PEN}),
	("GdkDeviceToolTypeEraser", #{const GDK_DEVICE_TOOL_TYPE_ERASER}),
	("GdkDeviceToolTypeBrush", #{const GDK_DEVICE_TOOL_TYPE_BRUSH}),
	("GdkDeviceToolTypePencil", #{const GDK_DEVICE_TOOL_TYPE_PENCIL}),
	("GdkDeviceToolTypeAirbrush", #{const GDK_DEVICE_TOOL_TYPE_AIRBRUSH}),
	("GdkDeviceToolTypeMouse", #{const GDK_DEVICE_TOOL_TYPE_MOUSE}),
	("GdkDeviceToolTypeLens", #{const GDK_DEVICE_TOOL_TYPE_LENS}) ]

gdkDeviceToolGetToolType :: GdkDeviceTool -> IO GdkDeviceToolType
gdkDeviceToolGetToolType t = GdkDeviceToolType <$> c_gdk_device_get_tool_type t

foreign import ccall "gdk_device_tool_get_tool_type" c_gdk_device_get_tool_type ::
	GdkDeviceTool -> IO #type GdkDeviceToolType
