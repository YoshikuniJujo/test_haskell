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
	IsGdkDevice(..),
	GdkDevice(..), GdkDeviceMaster(..), GdkDevicePhysical(..),

	PK(..), PointerOrKeyboard, filterPK,

	-- * DISPLAY AND SEAT
	gdkDeviceGetDisplay, gdkDeviceGetSeat,

	-- * IDENTITY
	gdkDeviceGetName,
	gdkDeviceGetVendorId,
	gdkDeviceGetProductId,
	gdkDeviceGetSource,
	gdkDeviceGetSourceInternal,
	gdkDeviceGetDeviceType,
	gdkDeviceGetDeviceTypeInternal,

	-- * SLAVES
	gdkDeviceListSlaveDevices,

	-- * GEOMETRY
	gdkDeviceWarp,
	gdkDeviceGetPosition, gdkDeviceGetPositionDouble,
	gdkDeviceGetWindowAtPosition, gdkDeviceGetWindowAtPositionDouble,
	gdkDeviceGetLastEventWindow,

	-- * GET N KEYS
	gdkDeviceGetNKeys,

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
import {-# SOURCE #-} Graphics.Gdk.Windows.Internal

#include <gdk/gdk.h>

enum "GdkInputSource" ''#{type GdkInputSource} [''Show, ''Read, ''Eq] [
	("GdkSourceMouse", #{const GDK_SOURCE_MOUSE}),
	("GdkSourcePen", #{const GDK_SOURCE_PEN}),
	("GdkSourceEraser", #{const GDK_SOURCE_ERASER}),
	("GdkSourceCursor", #{const GDK_SOURCE_CURSOR}),
	("GdkSourceKeyboard", #{const GDK_SOURCE_KEYBOARD}),
	("GdkSourceTouchscreen", #{const GDK_SOURCE_TOUCHSCREEN}),
	("GdkSourceTouchpad", #{const GDK_SOURCE_TOUCHPAD}),
	("GdkSourceTrackpoint", #{const GDK_SOURCE_TRACKPOINT}),
	("GdkSourceTabletPad", #{const GDK_SOURCE_TABLET_PAD}) ]

class IsGdkDevice (d :: PK -> *) where getGdkDevice :: d pk -> GdkDevice
newtype GdkDevice = GdkDevice (Ptr GdkDevice) deriving (Show, Storable)

newtype GdkDeviceMaster (pk :: PK) = GdkDeviceMaster (Ptr GdkDevice)
	deriving (Show, Storable)

instance IsGdkDevice GdkDeviceMaster where
	getGdkDevice (GdkDeviceMaster pd) = GdkDevice pd

newtype GdkDevicePhysical (pk :: PK)  = GdkDevicePhysical (Ptr GdkDevice)
	deriving (Show, Storable)

instance IsGdkDevice GdkDevicePhysical where
	getGdkDevice (GdkDevicePhysical pd) = GdkDevice pd

data PK = Pointer | Keyboard deriving Show

class PointerOrKeyboard (pk :: PK) where
	pointerOrKeyboard :: Ptr GdkDevice -> IO Bool

instance PointerOrKeyboard 'Pointer where
	pointerOrKeyboard d = (/= GdkSourceKeyboard)
		. GdkInputSource <$> c_gdk_device_get_source (GdkDevice d)

instance PointerOrKeyboard 'Keyboard where
	pointerOrKeyboard d = (== GdkSourceKeyboard)
		. GdkInputSource <$> c_gdk_device_get_source (GdkDevice d)

filterPK :: forall (pk :: PK) . PointerOrKeyboard pk =>
	[Ptr GdkDevice] -> IO [Ptr GdkDevice]
filterPK = filterM (pointerOrKeyboard @pk)

gdkDeviceGetName :: IsGdkDevice d => d pk -> IO String
gdkDeviceGetName d = peekCString =<< c_gdk_device_get_name (getGdkDevice d)

foreign import ccall "gdk_device_get_name" c_gdk_device_get_name ::
	GdkDevice -> IO CString

gdkDeviceGetVendorId :: GdkDevicePhysical pk -> Maybe String
gdkDeviceGetVendorId d = unsafePerformIO $ c_gdk_device_get_vendor_id d >>= \case
	NullPtr -> pure Nothing
	cs -> Just <$> peekCString cs

foreign import ccall "gdk_device_get_vendor_id" c_gdk_device_get_vendor_id ::
	GdkDevicePhysical pk -> IO CString

gdkDeviceGetProductId :: GdkDevicePhysical pk -> Maybe String
gdkDeviceGetProductId d = unsafePerformIO $ c_gdk_device_get_product_id d >>= \case
	NullPtr -> pure Nothing
	cs -> Just <$> peekCString cs

foreign import ccall "gdk_device_get_product_id" c_gdk_device_get_product_id ::
	GdkDevicePhysical pk -> IO CString

gdkDeviceGetSource :: IsGdkDevice d => d pk -> IO GdkInputSource
gdkDeviceGetSource d = GdkInputSource <$> c_gdk_device_get_source (getGdkDevice d)

gdkDeviceGetSourceInternal :: GdkDevice -> IO GdkInputSource
gdkDeviceGetSourceInternal d = GdkInputSource <$> c_gdk_device_get_source d

foreign import ccall "gdk_device_get_source" c_gdk_device_get_source ::
	GdkDevice -> IO #type GdkInputSource

gdkDeviceListSlaveDevices :: GdkDeviceMaster pk -> IO [GdkDevicePhysical pk]
gdkDeviceListSlaveDevices d = do
	gl <- c_gdk_device_list_slave_devices $ getGdkDevice d
	maybe (error "never occur") (map GdkDevicePhysical) <$> (g_list_to_list gl <* c_g_list_free gl)

foreign import ccall "gdk_device_list_slave_devices" c_gdk_device_list_slave_devices ::
	GdkDevice -> IO (Ptr (GList GdkDevice))

enum "GdkDeviceType" ''#{type GdkDeviceType} [''Show] [
	("GdkDeviceTypeMaster", #{const GDK_DEVICE_TYPE_MASTER}),
	("GdkDeviceTypeSlave", #{const GDK_DEVICE_TYPE_SLAVE}),
	("GdkDeviceTypeFloating", #{const GDK_DEVICE_TYPE_FLOATING}) ]

gdkDeviceGetDeviceType :: IsGdkDevice d => d pk -> IO GdkDeviceType
gdkDeviceGetDeviceType d = GdkDeviceType <$> c_gdk_device_get_device_type (getGdkDevice d)

gdkDeviceGetDeviceTypeInternal :: GdkDevice -> IO GdkDeviceType
gdkDeviceGetDeviceTypeInternal d = GdkDeviceType <$> c_gdk_device_get_device_type d

foreign import ccall "gdk_device_get_device_type" c_gdk_device_get_device_type ::
	GdkDevice -> IO #{type GdkDeviceType}

gdkDeviceGetDisplay :: IsGdkDevice d => d pk -> IO GdkDisplay
gdkDeviceGetDisplay = c_gdk_device_get_display . getGdkDevice

foreign import ccall "gdk_device_get_display"
	c_gdk_device_get_display :: GdkDevice -> IO GdkDisplay

foreign import ccall "gdk_device_warp" gdkDeviceWarp ::
	GdkDeviceMaster 'Pointer -> GdkScreen -> CInt -> CInt -> IO ()

gdkDeviceGetSeat :: IsGdkDevice d => d pk -> IO GdkSeat
gdkDeviceGetSeat = c_gdk_device_get_seat . getGdkDevice

foreign import ccall "gdk_device_get_seat"
	c_gdk_device_get_seat :: GdkDevice -> IO GdkSeat

gdkDeviceGetPosition :: GdkDeviceMaster 'Pointer -> IO (CInt, CInt)
gdkDeviceGetPosition d = alloca \px -> alloca \py -> do
	c_gdk_device_get_position (getGdkDevice d) NullPtr px py
	(,) <$> peek px <*> peek py

foreign import ccall "gdk_device_get_position" c_gdk_device_get_position ::
	GdkDevice -> Ptr (Ptr GdkScreen) -> Ptr CInt -> Ptr CInt -> IO ()

gdkDeviceGetPositionDouble ::
	GdkDeviceMaster 'Pointer -> IO (CDouble, CDouble)
gdkDeviceGetPositionDouble d = alloca \px -> alloca \py -> do
	c_gdk_device_get_position_double (getGdkDevice d) NullPtr px py
	(,) <$> peek px <*> peek py

foreign import ccall "gdk_device_get_position_double"
	c_gdk_device_get_position_double ::
	GdkDevice -> Ptr (Ptr GdkScreen) -> Ptr CDouble -> Ptr CDouble -> IO ()

gdkDeviceGetWindowAtPosition ::
	GdkDeviceMaster 'Pointer -> IO (Maybe (GdkWindow, (CInt, CInt)))
gdkDeviceGetWindowAtPosition d = alloca \px -> alloca \py -> do
	w <- c_gdk_device_get_window_at_position (getGdkDevice d) px py
	case w of
		GdkWindow NullPtr -> pure Nothing
		_ -> Just . (w ,) <$> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_window_at_position"
	c_gdk_device_get_window_at_position ::
	GdkDevice -> Ptr CInt -> Ptr CInt -> IO GdkWindow

gdkDeviceGetWindowAtPositionDouble ::
	GdkDeviceMaster 'Pointer -> IO (Maybe (GdkWindow, (CDouble, CDouble)))
gdkDeviceGetWindowAtPositionDouble d = alloca \px -> alloca \py -> do
	w <- c_gdk_device_get_window_at_position_double (getGdkDevice d) px py
	case w of
		GdkWindow NullPtr -> pure Nothing
		_ -> Just . (w ,) <$> ((,) <$> peek px <*> peek py)

foreign import ccall "gdk_device_get_window_at_position_double"
	c_gdk_device_get_window_at_position_double ::
	GdkDevice -> Ptr CDouble -> Ptr CDouble -> IO GdkWindow

gdkDeviceGetLastEventWindow :: GdkDeviceMaster 'Pointer -> IO (Maybe GdkWindow)
gdkDeviceGetLastEventWindow d =
	(<$> c_gdk_device_get_last_event_window (getGdkDevice d)) \case
		GdkWindow NullPtr -> Nothing; w -> Just w

foreign import ccall "gdk_device_get_last_event_window"
	c_gdk_device_get_last_event_window :: GdkDevice -> IO GdkWindow

gdkDeviceGetNKeys :: IsGdkDevice d => d 'Keyboard -> IO CInt
gdkDeviceGetNKeys = c_gdk_device_get_n_keys . getGdkDevice

foreign import ccall "gdk_device_get_n_keys"
	c_gdk_device_get_n_keys :: GdkDevice -> IO CInt
