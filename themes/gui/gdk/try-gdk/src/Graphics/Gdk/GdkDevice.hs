{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice (
	-- * GDK DEVICE
	-- ** IS GDK DEVICE
	IsGdkDevice, GdkDevice,
	-- ** IS GDK DEVICE MASTER
	GdkDeviceMasterPointer, GdkDeviceMasterKeyboard,
	-- ** GDK DEVICE PHYSICAL
	GdkDevicePhysical,

	-- * DISPLAY AND SEAT
	gdkDeviceGetDisplay, gdkDeviceGetSeat,

	-- * IDENTITY
	gdkDeviceGetName,
	gdkDeviceGetDeviceType,
	gdkDeviceGetSource,
	gdkDeviceGetVendorId,
	gdkDeviceGetProductId,

	-- * SLAVES
	gdkDeviceListSlaveDevices,

	-- * GEOMETRY
	gdkDeviceWarp,
	gdkDeviceGetPosition, gdkDeviceGetPositionDouble,
	gdkDeviceGetWindowAtPosition, gdkDeviceGetWindowAtPositionDouble,
	gdkDeviceGetLastEventWindow,

	-- * GDK DEVICE TYPE
	GdkDeviceType,
	pattern GdkDeviceTypeMaster, pattern GdkDeviceTypeSlave,
	pattern GdkDeviceTypeFloating,

	-- * GDK INPUT SOURCE
	GdkInputSource,
	pattern GdkSourceMouse, pattern GdkSourcePen, pattern GdkSourceEraser,
	pattern GdkSourceCursor, pattern GdkSourceKeyboard,
	pattern GdkSourceTouchscreen, pattern GdkSourceTouchpad,
	pattern GdkSourceTrackpoint, pattern GdkSourceTabletPad,

	) where

import Graphics.Gdk.GdkDevice.Internal
