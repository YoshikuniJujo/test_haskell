{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice.Internal (
	-- * GDK DEVICE
	GetGdkDevice (..), IsGdkDevice(..),
	GdkDevice(..), GdkDevicePhysical(..),
	IsGdkDeviceMaster(..),
	GdkDeviceMasterPointer(..), GdkDeviceMasterKeyboard(..),

	-- * DISPLAY AND SEAT
	gdkDeviceGetDisplay, gdkDeviceGetSeat,

	-- * IDENTITY
	gdkDeviceGetName,
	gdkDeviceGetVendorId,
	gdkDeviceGetProductId,
	gdkDeviceGetSource,
	gdkDeviceGetDeviceType,

	-- * SLAVES
	gdkDeviceListSlaveDevices,

	-- * GEOMETRY
	gdkDeviceWarp,
	gdkDeviceGetPosition, gdkDeviceGetPositionDouble,
	gdkDeviceGetWindowAtPosition, gdkDeviceGetWindowAtPositionDouble,

	-- * STATE
	gdkDeviceGetHasCursor,
	gdkDeviceGetLastEventWindow,

	-- * GDK DEVICE TYPE
	GdkDeviceType(..),
	pattern GdkDeviceTypeMaster, pattern GdkDeviceTypeSlave,
	pattern GdkDeviceTypeFloating,

	-- * GDK INPUT SOURCE
	GdkInputSource(..),
	pattern GdkSourceMouse, pattern GdkSourcePen, pattern GdkSourceEraser,
	pattern GdkSourceCursor, pattern GdkSourceKeyboard,
	pattern GdkSourceTouchscreen, pattern GdkSourceTouchpad,
	pattern GdkSourceTrackpoint, pattern GdkSourceTabletPad,

	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Foreign.C.Enum
import Data.Word
import Data.Int
import System.IO.Unsafe
import System.GLib.DoublyLinkedLists
import System.GLib.Bool

import {-# SOURCE #-} Graphics.Gdk.GdkDisplay.Internal
import {-# SOURCE #-} Graphics.Gdk.GdkScreen.Internal
import {-# SOURCE #-} Graphics.Gdk.GdkSeat.Internal
import {-# SOURCE #-} Graphics.Gdk.Windows

#include <gdk/gdk.h>

class GetGdkDevice d where getGdkDevice :: d -> GdkDevice

newtype GdkDevice = GdkDevice (Ptr GdkDevice) deriving (Show, Storable)

class IsGdkDevice d where toGdkDevice :: d -> SomeGdkDevice

data SomeGdkDevice
	= SomeGdkDeviceMaster SomeGdkDeviceMaster
	| SomeGdkDevicePhysical GdkDevicePhysical
	deriving Show

instance GetGdkDevice SomeGdkDevice where
	getGdkDevice = \case
		SomeGdkDeviceMaster dm -> getGdkDevice dm
		SomeGdkDevicePhysical dp -> getGdkDevice dp

instance IsGdkDevice SomeGdkDevice where toGdkDevice = id

class IsGdkDeviceMaster dm where toGdkDeviceMaster :: dm -> SomeGdkDeviceMaster

data SomeGdkDeviceMaster
	= SomeGdkDeviceMasterPointer GdkDeviceMasterPointer
	| SomeGdkDeviceMasterKeyboard GdkDeviceMasterKeyboard
	deriving Show

instance GetGdkDevice SomeGdkDeviceMaster where
	getGdkDevice = \case
		SomeGdkDeviceMasterPointer dmp -> getGdkDevice dmp
		SomeGdkDeviceMasterKeyboard dmk -> getGdkDevice dmk

instance IsGdkDevice SomeGdkDeviceMaster where toGdkDevice = SomeGdkDeviceMaster
instance IsGdkDeviceMaster SomeGdkDeviceMaster where toGdkDeviceMaster = id

newtype GdkDeviceMasterPointer = GdkDeviceMasterPointer (Ptr GdkDevice)
	deriving (Show, Storable)

instance GetGdkDevice GdkDeviceMasterPointer where
	getGdkDevice (GdkDeviceMasterPointer pd) = GdkDevice pd

instance IsGdkDevice GdkDeviceMasterPointer where
	toGdkDevice = toGdkDevice . toGdkDeviceMaster

instance IsGdkDeviceMaster GdkDeviceMasterPointer where
	toGdkDeviceMaster = SomeGdkDeviceMasterPointer

newtype GdkDeviceMasterKeyboard = GdkDeviceMasterKeyboard (Ptr GdkDevice)
	deriving (Show, Storable)

instance GetGdkDevice GdkDeviceMasterKeyboard where
	getGdkDevice (GdkDeviceMasterKeyboard pd) = GdkDevice pd

instance IsGdkDevice GdkDeviceMasterKeyboard where
	toGdkDevice = toGdkDevice . toGdkDeviceMaster

instance IsGdkDeviceMaster GdkDeviceMasterKeyboard where
	toGdkDeviceMaster = SomeGdkDeviceMasterKeyboard

newtype GdkDevicePhysical = GdkDevicePhysical (Ptr GdkDevice)
	deriving (Show, Storable)

instance GetGdkDevice GdkDevicePhysical where
	getGdkDevice (GdkDevicePhysical pd) = GdkDevice pd

instance IsGdkDevice GdkDevicePhysical where toGdkDevice = SomeGdkDevicePhysical

gdkDeviceGetName :: IsGdkDevice d => d -> IO String
gdkDeviceGetName d = peekCString =<< c_gdk_device_get_name (getGdkDevice $ toGdkDevice d)

foreign import ccall "gdk_device_get_name" c_gdk_device_get_name ::
	GdkDevice -> IO CString

gdkDeviceGetVendorId :: GdkDevicePhysical -> String
gdkDeviceGetVendorId d = unsafePerformIO $ c_gdk_device_get_vendor_id d >>= \case
	NullPtr -> error "never occur"
	cs -> peekCString cs

foreign import ccall "gdk_device_get_vendor_id" c_gdk_device_get_vendor_id ::
	GdkDevicePhysical -> IO CString

gdkDeviceGetProductId :: GdkDevicePhysical -> String
gdkDeviceGetProductId d = unsafePerformIO $ c_gdk_device_get_product_id d >>= \case
	NullPtr -> error "never occur"; cs -> peekCString cs

foreign import ccall "gdk_device_get_product_id" c_gdk_device_get_product_id ::
	GdkDevicePhysical -> IO CString

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

gdkDeviceGetSource :: IsGdkDevice d => d -> IO GdkInputSource
gdkDeviceGetSource d = GdkInputSource <$> c_gdk_device_get_source (getGdkDevice $ toGdkDevice d)

foreign import ccall "gdk_device_get_source" c_gdk_device_get_source ::
	GdkDevice -> IO #type GdkInputSource

gdkDeviceListSlaveDevices :: IsGdkDeviceMaster d => d -> IO [GdkDevicePhysical]
gdkDeviceListSlaveDevices d = do
	gl <- c_gdk_device_list_slave_devices . getGdkDevice . toGdkDevice $ toGdkDeviceMaster d
	maybe (error "never occur") (map GdkDevicePhysical) <$> (g_list_to_list gl <* c_g_list_free gl)

foreign import ccall "gdk_device_list_slave_devices" c_gdk_device_list_slave_devices ::
	GdkDevice -> IO (Ptr (GList GdkDevice))

enum "GdkDeviceType" ''#{type GdkDeviceType} [''Show] [
	("GdkDeviceTypeMaster", #{const GDK_DEVICE_TYPE_MASTER}),
	("GdkDeviceTypeSlave", #{const GDK_DEVICE_TYPE_SLAVE}),
	("GdkDeviceTypeFloating", #{const GDK_DEVICE_TYPE_FLOATING}) ]

gdkDeviceGetDeviceType :: IsGdkDevice d => d -> IO GdkDeviceType
gdkDeviceGetDeviceType d = GdkDeviceType <$> c_gdk_device_get_device_type (getGdkDevice $ toGdkDevice d)

foreign import ccall "gdk_device_get_device_type" c_gdk_device_get_device_type ::
	GdkDevice -> IO #{type GdkDeviceType}

gdkDeviceGetDisplay :: IsGdkDevice d => d -> IO GdkDisplay
gdkDeviceGetDisplay = c_gdk_device_get_display . getGdkDevice . toGdkDevice

foreign import ccall "gdk_device_get_display"
	c_gdk_device_get_display :: GdkDevice -> IO GdkDisplay

gdkDeviceGetHasCursor :: IsGdkDevice d => d -> IO Bool
gdkDeviceGetHasCursor d = gbooleanToBool <$> c_gdk_device_get_has_cursor (getGdkDevice $ toGdkDevice d)

foreign import ccall "gdk_device_get_has_cursor" c_gdk_device_get_has_cursor ::
	GdkDevice -> IO #{type gboolean}

foreign import ccall "gdk_device_warp" gdkDeviceWarp ::
	GdkDeviceMasterPointer -> GdkScreen -> CInt -> CInt -> IO ()

gdkDeviceGetSeat :: IsGdkDevice d => d -> IO GdkSeat
gdkDeviceGetSeat = c_gdk_device_get_seat . getGdkDevice . toGdkDevice

foreign import ccall "gdk_device_get_seat"
	c_gdk_device_get_seat :: GdkDevice -> IO GdkSeat

gdkDeviceGetPosition :: GdkDeviceMasterPointer -> IO (GdkScreen, (CInt, CInt))
gdkDeviceGetPosition d = alloca \pps -> alloca \px -> alloca \py -> do
	c_gdk_device_get_position (getGdkDevice d) pps px py
	(,) <$> (GdkScreen <$> peek pps) <*> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_position" c_gdk_device_get_position ::
	GdkDevice -> Ptr (Ptr GdkScreen) -> Ptr CInt -> Ptr CInt -> IO ()

gdkDeviceGetPositionDouble ::
	GdkDeviceMasterPointer -> IO (GdkScreen, (CDouble, CDouble))
gdkDeviceGetPositionDouble d = alloca \pps -> alloca \px -> alloca \py -> do
	c_gdk_device_get_position_double (getGdkDevice d) pps px py
	(,) <$> (GdkScreen <$> peek pps) <*> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_position_double"
	c_gdk_device_get_position_double ::
	GdkDevice -> Ptr (Ptr GdkScreen) -> Ptr CDouble -> Ptr CDouble -> IO ()

gdkDeviceGetWindowAtPosition ::
	GdkDeviceMasterPointer -> IO (GdkWindow, (CInt, CInt))
gdkDeviceGetWindowAtPosition d = alloca \px -> alloca \py -> do
	w <- c_gdk_device_get_window_at_position (getGdkDevice d) px py
	(w ,) <$> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_window_at_position"
	c_gdk_device_get_window_at_position ::
	GdkDevice -> Ptr CInt -> Ptr CInt -> IO GdkWindow

gdkDeviceGetWindowAtPositionDouble ::
	GdkDeviceMasterPointer -> IO (GdkWindow, (CDouble, CDouble))
gdkDeviceGetWindowAtPositionDouble d = alloca \px -> alloca \py -> do
	w <- c_gdk_device_get_window_at_position_double (getGdkDevice d) px py
	(w ,) <$> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_window_at_position_double"
	c_gdk_device_get_window_at_position_double ::
	GdkDevice -> Ptr CDouble -> Ptr CDouble -> IO GdkWindow

foreign import ccall "gdk_device_get_last_event_window"
	gdkDeviceGetLastEventWindow :: GdkDevice -> IO GdkWindow
