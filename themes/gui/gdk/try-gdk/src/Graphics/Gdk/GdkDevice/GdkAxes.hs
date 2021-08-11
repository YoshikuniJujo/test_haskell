{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice.GdkAxes (
	-- * GDK AXES
	GdkAxes,

	-- * NUMBER AND AVAILABLE
	gdkDeviceGetNAxes,
	gdkDeviceGetAxes,

	-- * WITH AXIS USE
	gdkDeviceSetAxisUse,
	gdkDeviceGetAxisUse,
	gdkDeviceGetAxis,

	-- * WITH ATOMS
	gdkDeviceListAxes,
	gdkDeviceGetAxisValue,

	-- * GDK AXIS USE
	GdkAxisUse,
	pattern GdkAxisIgnore,
	pattern GdkAxisX, pattern GdkAxisY,
	pattern GdkAxisPressure,
	pattern GdkAxisXtilt, pattern GdkAxisYtilt,
	pattern GdkAxisWheel, pattern GdkAxisDistance,
	pattern GdkAxisRotation, pattern GdkAxisSlider,
	pattern GdkAxisLast,

	-- * GDK AXIS FLAGS
	-- ** Multiple Flags
	GdkAxisFlags, gdkAxisFlags,
	pattern GdkAxisNoFlags,

	-- ** Single Flag
	GdkAxisFlag,
	gdkAxisFlagList,
	pattern GdkAxisFlagX, pattern GdkAxisFlagY,
	pattern GdkAxisFlagPressure,
	pattern GdkAxisFlagXtilt, pattern GdkAxisFlagYtilt,
	pattern GdkAxisFlagWheel, pattern GdkAxisFlagDistance,
	pattern GdkAxisFlagRotation, pattern GdkAxisFlagSlider

	) where

import Graphics.Gdk.GdkDevice.GdkAxes.Internal
