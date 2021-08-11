{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice (
	-- * GDK DEVICE
	IsGdkDevice, GdkDeviceMaster, GdkDevicePhysical, PK(..),

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

	-- * GET N KEYS
	gdkDeviceGetNKeys,

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
