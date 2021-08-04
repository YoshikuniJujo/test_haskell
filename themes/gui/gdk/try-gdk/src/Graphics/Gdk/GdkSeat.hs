{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkSeat (
	-- * GDK SEAT
	GdkSeat,

	-- * GET

	gdkSeatGetDisplay,
	gdkSeatGetCapabilities,
	gdkSeatGetPointer, gdkSeatGetKeyboard,
	gdkSeatGetSlaves,

	-- * GRAB

	gdkSeatGrab, gdkSeatGrabSimple, gdkSeatUngrab,
	GdkSeatGrabPrepareFunc, noGdkSeatGrabPrepare,

	-- * GDK SEAT CAPABILITIES

	GdkSeatCapabilities,
	pattern GdkSeatCapabilityNone,
	pattern GdkSeatCapabilityAllPointing,
	pattern GdkSeatCapabilityAll,

	gdkSeatCapabilities,

	GdkSeatCapability,
	pattern GdkSeatCapabilityPointer,
	pattern GdkSeatCapabilityTouch,
	pattern GdkSeatCapabilityTabletStylus,
	pattern GdkSeatCapabilityKeyboard,

	gdkSeatCapabilityList,

	) where

import Graphics.Gdk.GdkSeat.Internal

noGdkSeatGrabPrepare :: Maybe (GdkSeatGrabPrepareFunc (), ())
noGdkSeatGrabPrepare = Nothing
