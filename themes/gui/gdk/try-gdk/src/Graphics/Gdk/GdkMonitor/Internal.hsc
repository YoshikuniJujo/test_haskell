{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkMonitor.Internal (

	-- * GDK MONITOR
	GdkMonitor(..),

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
	GdkSubpixelLayout(..),
	pattern GdkSubpixelLayoutUnknown,
	pattern GdkSubpixelLayoutNone,
	pattern GdkSubpixelLayoutHorizontalRgb,
	pattern GdkSubpixelLayoutHorizontalBgr,
	pattern GdkSubpixelLayoutVerticalRgb,
	pattern GdkSubpixelLayoutVerticalBgr

	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr
import Foreign.C
import Foreign.C.Enum
import Data.Word
import Data.Int
import System.GLib.Bool

import {-# SOURCE #-} Graphics.Gdk.GdkDisplay.Internal
import Graphics.Gdk.PointsAndRectangles.Internal

#include <gdk/gdk.h>

newtype GdkMonitor = GdkMonitor (Ptr GdkMonitor) deriving Show

gdkMonitorGetDisplay :: GdkMonitor -> IO GdkDisplay
gdkMonitorGetDisplay (GdkMonitor p) = GdkDisplay <$> c_gdk_monitor_get_display p

foreign import ccall "gdk_monitor_get_display" c_gdk_monitor_get_display ::
	Ptr GdkMonitor -> IO (Ptr GdkDisplay)

gdkMonitorGetGeometry :: GdkMonitor -> IO GdkRectangle
gdkMonitorGetGeometry (GdkMonitor m) = do
	r@(GdkRectanglePrim fr) <- gdkRectangleNew
	withForeignPtr fr $ c_gdk_monitor_get_geometry m
	gdkRectangleFreeze r

foreign import ccall "gdk_monitor_get_geometry" c_gdk_monitor_get_geometry ::
	Ptr GdkMonitor -> Ptr GdkRectangle -> IO ()

gdkMonitorGetWorkarea :: GdkMonitor -> IO GdkRectangle
gdkMonitorGetWorkarea (GdkMonitor m) = do
	r@(GdkRectanglePrim fr) <- gdkRectangleNew
	withForeignPtr fr $ c_gdk_monitor_get_workarea m
	gdkRectangleFreeze r

foreign import ccall "gdk_monitor_get_workarea" c_gdk_monitor_get_workarea ::
	Ptr GdkMonitor -> Ptr GdkRectangle -> IO()

gdkMonitorGetWidthMm, gdkMonitorGetHeightMm :: GdkMonitor -> IO CInt
gdkMonitorGetWidthMm (GdkMonitor p) = c_gdk_monitor_get_width_mm p
gdkMonitorGetHeightMm (GdkMonitor p) = c_gdk_monitor_get_height_mm p

foreign import ccall "gdk_monitor_get_width_mm" c_gdk_monitor_get_width_mm ::
	Ptr GdkMonitor -> IO CInt

foreign import ccall "gdk_monitor_get_height_mm" c_gdk_monitor_get_height_mm ::
	Ptr GdkMonitor -> IO CInt

gdkMonitorGetManufacturer :: GdkMonitor -> IO (Maybe String)
gdkMonitorGetManufacturer (GdkMonitor p) =
	c_gdk_monitor_get_manufacturer p >>= \case
		NullPtr -> pure Nothing; cs -> Just <$> peekCString cs

foreign import ccall "gdk_monitor_get_manufacturer"
	c_gdk_monitor_get_manufacturer :: Ptr GdkMonitor -> IO CString

gdkMonitorGetModel :: GdkMonitor -> IO (Maybe String)
gdkMonitorGetModel (GdkMonitor p) = c_gdk_monitor_get_model p >>= \case
	NullPtr -> pure Nothing; cs -> Just <$> peekCString cs

foreign import ccall "gdk_monitor_get_model" c_gdk_monitor_get_model ::
	Ptr GdkMonitor -> IO CString

gdkMonitorGetScaleFactor :: GdkMonitor -> IO CInt
gdkMonitorGetScaleFactor (GdkMonitor p) = c_gdk_monitor_get_scale_factor p

foreign import ccall "gdk_monitor_get_scale_factor" c_gdk_monitor_get_scale_factor ::
	Ptr GdkMonitor -> IO CInt

gdkMonitorGetRefreshRate :: GdkMonitor -> IO CInt
gdkMonitorGetRefreshRate (GdkMonitor p) = c_gdk_monitor_get_refresh_rate p

foreign import ccall "gdk_monitor_get_refresh_rate" c_gdk_monitor_get_refresh_rate ::
	Ptr GdkMonitor -> IO CInt

enum "GdkSubpixelLayout" ''#{type GdkSubpixelLayout} [''Show] [
	("GdkSubpixelLayoutUnknown", #{const GDK_SUBPIXEL_LAYOUT_UNKNOWN}),
	("GdkSubpixelLayoutNone", #{const GDK_SUBPIXEL_LAYOUT_NONE}),
	("GdkSubpixelLayoutHorizontalRgb",
		#{const GDK_SUBPIXEL_LAYOUT_HORIZONTAL_RGB}),
	("GdkSubpixelLayoutHorizontalBgr",
		#{const GDK_SUBPIXEL_LAYOUT_HORIZONTAL_BGR}),
	("GdkSubpixelLayoutVerticalRgb",
		#{const GDK_SUBPIXEL_LAYOUT_VERTICAL_RGB}),
	("GdkSubpixelLayoutVerticalBgr",
		#{const GDK_SUBPIXEL_LAYOUT_VERTICAL_BGR}) ]

gdkMonitorGetSubpixelLayout :: GdkMonitor -> IO GdkSubpixelLayout
gdkMonitorGetSubpixelLayout (GdkMonitor p) =
	GdkSubpixelLayout <$> c_gdk_monitor_get_subpixel_layout p

foreign import ccall "gdk_monitor_get_subpixel_layout"
	c_gdk_monitor_get_subpixel_layout ::
	Ptr GdkMonitor -> IO #type GdkSubpixelLayout

gdkMonitorIsPrimary :: GdkMonitor -> IO Bool
gdkMonitorIsPrimary (GdkMonitor p) =
	gbooleanToBool <$> c_gdk_monitor_is_primary p

foreign import ccall "gdk_monitor_is_primary" c_gdk_monitor_is_primary ::
	Ptr GdkMonitor -> IO #type gboolean
