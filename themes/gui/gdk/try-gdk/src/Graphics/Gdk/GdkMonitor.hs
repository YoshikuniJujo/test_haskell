{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkMonitor (

	-- * GDK MONITOR
	GdkMonitor,

	-- * DISPLAY
	gdkMonitorGetDisplay,

	-- * GEOMETRY
	gdkMonitorGetGeometry,
	gdkMonitorGetWorkarea,
	gdkMonitorGetWidthMm,
	gdkMonitorGetHeightMm,

	-- * MANUFACTURER AND MODEL
	gdkMonitorGetManufacturer,
	gdkMonitorGetModel,

	-- * PROPERTIES
	gdkMonitorGetScaleFactor,
	gdkMonitorGetRefreshRate,
	gdkMonitorGetSubpixelLayout,

	-- * IS PRIMARY
	gdkMonitorIsPrimary,

	-- * GDK SUBPIXEL LAYOUT
	GdkSubpixelLayout,
	pattern GdkSubpixelLayoutUnknown,
	pattern GdkSubpixelLayoutNone,
	pattern GdkSubpixelLayoutHorizontalRgb,
	pattern GdkSubpixelLayoutHorizontalBgr,
	pattern GdkSubpixelLayoutVerticalRgb,
	pattern GdkSubpixelLayoutVerticalBgr

	) where

import Graphics.Gdk.GdkMonitor.Internal
