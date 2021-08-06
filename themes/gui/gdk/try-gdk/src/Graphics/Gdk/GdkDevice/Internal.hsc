{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice.Internal (
	-- * GDK DEVICE
	GetGdkDevice (..), IsGdkDevice(..),
	GdkDevice(..), GdkDevicePhysical(..),
	IsGdkDeviceMaster(..),
	GdkDeviceMasterPointer(..), GdkDeviceMasterKeyboard,

	PK(..), PointerOrKeyboard, filterPK,

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
import Control.Monad
import System.IO.Unsafe
import System.GLib.DoublyLinkedLists

import {-# SOURCE #-} Graphics.Gdk.GdkDisplay.Internal
import {-# SOURCE #-} Graphics.Gdk.GdkScreen.Internal
import {-# SOURCE #-} Graphics.Gdk.GdkSeat.Internal
import {-# SOURCE #-} Graphics.Gdk.Windows

#include <gdk/gdk.h>

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

data PK = Pointer | Keyboard deriving Show

class PointerOrKeyboard (pk :: PK) where
	pointerOrKeyboard :: Ptr GdkDevice -> IO Bool

instance PointerOrKeyboard Pointer where
	pointerOrKeyboard d = (/= GdkSourceKeyboard) . GdkInputSource <$> c_gdk_device_get_source (GdkDevice d)

instance PointerOrKeyboard Keyboard where
	pointerOrKeyboard d = (== GdkSourceKeyboard) . GdkInputSource <$> c_gdk_device_get_source (GdkDevice d)

filterPK :: forall (pk :: PK) . PointerOrKeyboard pk => [Ptr GdkDevice] -> IO [Ptr GdkDevice]
filterPK = filterM (pointerOrKeyboard @pk)

class GetGdkDevice d where getGdkDevice :: d -> GdkDevice

newtype GdkDevice = GdkDevice (Ptr GdkDevice) deriving (Show, Storable)

class IsGdkDevice (d :: PK -> *) where toGdkDevice :: d pk -> SomeGdkDevice pk

data SomeGdkDevice pk
	= SomeGdkDeviceMaster (SomeGdkDeviceMaster pk)
	| SomeGdkDevicePhysical (GdkDevicePhysical pk)
	deriving Show

instance GetGdkDevice (SomeGdkDevice pk) where
	getGdkDevice = \case
		SomeGdkDeviceMaster dm -> getGdkDevice dm
		SomeGdkDevicePhysical dp -> getGdkDevice dp

instance IsGdkDevice SomeGdkDevice where toGdkDevice = id

class IsGdkDeviceMaster dm where toGdkDeviceMaster :: dm pk -> SomeGdkDeviceMaster pk

data SomeGdkDeviceMaster pk
	= SomeGdkDeviceMasterPointer (GdkDeviceMasterPointer pk)
	deriving Show

instance GetGdkDevice (SomeGdkDeviceMaster pk) where
	getGdkDevice = \case
		SomeGdkDeviceMasterPointer dmp -> getGdkDevice dmp

instance IsGdkDevice SomeGdkDeviceMaster where toGdkDevice = SomeGdkDeviceMaster
instance IsGdkDeviceMaster SomeGdkDeviceMaster where toGdkDeviceMaster = id

newtype GdkDeviceMasterPointer (pk :: PK) = GdkDeviceMasterPointer (Ptr GdkDevice)
	deriving (Show, Storable)

instance GetGdkDevice (GdkDeviceMasterPointer pk) where
	getGdkDevice (GdkDeviceMasterPointer pd) = GdkDevice pd

instance IsGdkDevice GdkDeviceMasterPointer where
	toGdkDevice = toGdkDevice . toGdkDeviceMaster

instance IsGdkDeviceMaster GdkDeviceMasterPointer where
	toGdkDeviceMaster = SomeGdkDeviceMasterPointer

type GdkDeviceMasterKeyboard = GdkDeviceMasterPointer

newtype GdkDevicePhysical (pk :: PK)  = GdkDevicePhysical (Ptr GdkDevice)
	deriving (Show, Storable)

instance GetGdkDevice (GdkDevicePhysical pk) where
	getGdkDevice (GdkDevicePhysical pd) = GdkDevice pd

instance IsGdkDevice GdkDevicePhysical where toGdkDevice = SomeGdkDevicePhysical

gdkDeviceGetName :: IsGdkDevice d => d pk -> IO String
gdkDeviceGetName d = peekCString =<< c_gdk_device_get_name (getGdkDevice $ toGdkDevice d)

foreign import ccall "gdk_device_get_name" c_gdk_device_get_name ::
	GdkDevice -> IO CString

gdkDeviceGetVendorId :: GdkDevicePhysical pk -> String
gdkDeviceGetVendorId d = unsafePerformIO $ c_gdk_device_get_vendor_id d >>= \case
	NullPtr -> error "never occur"
	cs -> peekCString cs

foreign import ccall "gdk_device_get_vendor_id" c_gdk_device_get_vendor_id ::
	GdkDevicePhysical pk -> IO CString

gdkDeviceGetProductId :: GdkDevicePhysical pk -> String
gdkDeviceGetProductId d = unsafePerformIO $ c_gdk_device_get_product_id d >>= \case
	NullPtr -> error "never occur"; cs -> peekCString cs

foreign import ccall "gdk_device_get_product_id" c_gdk_device_get_product_id ::
	GdkDevicePhysical pk -> IO CString

gdkDeviceGetSource :: IsGdkDevice d => d pk -> IO GdkInputSource
gdkDeviceGetSource d = GdkInputSource <$> c_gdk_device_get_source (getGdkDevice $ toGdkDevice d)

foreign import ccall "gdk_device_get_source" c_gdk_device_get_source ::
	GdkDevice -> IO #type GdkInputSource

gdkDeviceListSlaveDevices :: IsGdkDeviceMaster d => d pk -> IO [GdkDevicePhysical pk]
gdkDeviceListSlaveDevices d = do
	gl <- c_gdk_device_list_slave_devices . getGdkDevice . toGdkDevice $ toGdkDeviceMaster d
	maybe (error "never occur") (map GdkDevicePhysical) <$> (g_list_to_list gl <* c_g_list_free gl)

foreign import ccall "gdk_device_list_slave_devices" c_gdk_device_list_slave_devices ::
	GdkDevice -> IO (Ptr (GList GdkDevice))

enum "GdkDeviceType" ''#{type GdkDeviceType} [''Show] [
	("GdkDeviceTypeMaster", #{const GDK_DEVICE_TYPE_MASTER}),
	("GdkDeviceTypeSlave", #{const GDK_DEVICE_TYPE_SLAVE}),
	("GdkDeviceTypeFloating", #{const GDK_DEVICE_TYPE_FLOATING}) ]

gdkDeviceGetDeviceType :: IsGdkDevice d => d pk -> IO GdkDeviceType
gdkDeviceGetDeviceType d = GdkDeviceType <$> c_gdk_device_get_device_type (getGdkDevice $ toGdkDevice d)

foreign import ccall "gdk_device_get_device_type" c_gdk_device_get_device_type ::
	GdkDevice -> IO #{type GdkDeviceType}

gdkDeviceGetDisplay :: IsGdkDevice d => d pk -> IO GdkDisplay
gdkDeviceGetDisplay = c_gdk_device_get_display . getGdkDevice . toGdkDevice

foreign import ccall "gdk_device_get_display"
	c_gdk_device_get_display :: GdkDevice -> IO GdkDisplay

foreign import ccall "gdk_device_warp" gdkDeviceWarp ::
	GdkDeviceMasterPointer pk -> GdkScreen -> CInt -> CInt -> IO ()

gdkDeviceGetSeat :: IsGdkDevice d => d pk -> IO GdkSeat
gdkDeviceGetSeat = c_gdk_device_get_seat . getGdkDevice . toGdkDevice

foreign import ccall "gdk_device_get_seat"
	c_gdk_device_get_seat :: GdkDevice -> IO GdkSeat

gdkDeviceGetPosition :: GdkDeviceMasterPointer pk -> IO (GdkScreen, (CInt, CInt))
gdkDeviceGetPosition d = alloca \pps -> alloca \px -> alloca \py -> do
	c_gdk_device_get_position (getGdkDevice d) pps px py
	(,) <$> (GdkScreen <$> peek pps) <*> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_position" c_gdk_device_get_position ::
	GdkDevice -> Ptr (Ptr GdkScreen) -> Ptr CInt -> Ptr CInt -> IO ()

gdkDeviceGetPositionDouble ::
	GdkDeviceMasterPointer pk -> IO (GdkScreen, (CDouble, CDouble))
gdkDeviceGetPositionDouble d = alloca \pps -> alloca \px -> alloca \py -> do
	c_gdk_device_get_position_double (getGdkDevice d) pps px py
	(,) <$> (GdkScreen <$> peek pps) <*> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_position_double"
	c_gdk_device_get_position_double ::
	GdkDevice -> Ptr (Ptr GdkScreen) -> Ptr CDouble -> Ptr CDouble -> IO ()

gdkDeviceGetWindowAtPosition ::
	GdkDeviceMasterPointer pk -> IO (GdkWindow, (CInt, CInt))
gdkDeviceGetWindowAtPosition d = alloca \px -> alloca \py -> do
	w <- c_gdk_device_get_window_at_position (getGdkDevice d) px py
	(w ,) <$> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_window_at_position"
	c_gdk_device_get_window_at_position ::
	GdkDevice -> Ptr CInt -> Ptr CInt -> IO GdkWindow

gdkDeviceGetWindowAtPositionDouble ::
	GdkDeviceMasterPointer pk -> IO (GdkWindow, (CDouble, CDouble))
gdkDeviceGetWindowAtPositionDouble d = alloca \px -> alloca \py -> do
	w <- c_gdk_device_get_window_at_position_double (getGdkDevice d) px py
	(w ,) <$> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_window_at_position_double"
	c_gdk_device_get_window_at_position_double ::
	GdkDevice -> Ptr CDouble -> Ptr CDouble -> IO GdkWindow

gdkDeviceGetLastEventWindow :: GdkDeviceMasterPointer pk -> IO GdkWindow
gdkDeviceGetLastEventWindow =
	c_gdk_device_get_last_event_window . getGdkDevice

foreign import ccall "gdk_device_get_last_event_window"
	c_gdk_device_get_last_event_window :: GdkDevice -> IO GdkWindow
