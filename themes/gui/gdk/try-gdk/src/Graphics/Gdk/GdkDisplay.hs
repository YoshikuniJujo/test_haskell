{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDisplay (
	-- * TYPE
	GdkDisplay,

	-- * DISPLAY
	gdkDisplayOpen,
	gdkDisplayGetDefault,
	gdkDisplayGetName,
	gdkDisplayGetDefaultScreen,
	gdkDisplayDeviceIsGrabbed,
	gdkDisplaySync,
	gdkDisplayFlush,
	gdkDisplayClose,
	gdkDisplayIsClosed,

	-- * EVENT
	gdkDisplayWithEvent,

	-- * DOUBLE CLICK
	gdkDisplaySetDoubleClickTime,
	gdkDisplaySetDoubleClickDistance,

	-- * CURSOR
	gdkDisplaySupportsCursorColor,
	gdkDisplaySupportsCursorAlpha,
	gdkDisplayGetDefaultCursorSize,

	-- * GROUP
	gdkDisplayGetDefaultGroup,

	-- * SEAT
	gdkDisplayGetDefaultSeat,
	gdkDisplayListSeats,

	-- * MONITOR
	gdkDisplayGetNMonitors,
	gdkDisplayGetMonitor,
	gdkDisplayGetPrimaryMonitor,
	gdkDisplayGetMonitorAtPoint,
	gdkDisplayGetMonitorAtWindow ) where

import Graphics.Gdk.GdkDisplay.Internal
