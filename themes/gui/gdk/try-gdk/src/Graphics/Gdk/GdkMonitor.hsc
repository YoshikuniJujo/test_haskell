{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkMonitor where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Data.Word
import Data.Int
import System.GLib.Bool

import Graphics.Gdk.Types
import Graphics.Gdk.Values

#include <gdk/gdk.h>

foreign import ccall "gdk_monitor_get_display" c_gdk_monitor_get_display ::
	Ptr GdkMonitor -> IO (Ptr GdkDisplay)

gdkMonitorGetDisplay :: GdkMonitor -> IO GdkDisplay
gdkMonitorGetDisplay (GdkMonitor p) = GdkDisplay <$> c_gdk_monitor_get_display p

foreign import ccall "gdk_monitor_get_geometry" c_gdk_monitor_get_geometry ::
	Ptr GdkMonitor -> Ptr GdkRectangle -> IO ()

gdkMonitorGetGeometry :: GdkMonitor -> IO GdkRectangle
gdkMonitorGetGeometry (GdkMonitor m) = alloca \r ->
	c_gdk_monitor_get_geometry m r >> peek r

foreign import ccall "gdk_monitor_get_workarea" c_gdk_monitor_get_workarea ::
	Ptr GdkMonitor -> Ptr GdkRectangle -> IO()

gdkMonitorGetWorkarea :: GdkMonitor -> IO GdkRectangle
gdkMonitorGetWorkarea (GdkMonitor m) = alloca \r ->
	c_gdk_monitor_get_workarea m r >> peek r

foreign import ccall "gdk_monitor_get_width_mm" c_gdk_monitor_get_width_mm ::
	Ptr GdkMonitor -> IO #type int

foreign import ccall "gdk_monitor_get_height_mm" c_gdk_monitor_get_height_mm ::
	Ptr GdkMonitor -> IO #type int

gdkMonitorGetWidthMm, gdkMonitorGetHeightMm :: GdkMonitor -> IO #type int
gdkMonitorGetWidthMm (GdkMonitor p) = c_gdk_monitor_get_width_mm p
gdkMonitorGetHeightMm (GdkMonitor p) = c_gdk_monitor_get_height_mm p

foreign import ccall "gdk_monitor_get_manufacturer" c_gdk_monitor_get_manufacturer ::
	Ptr GdkMonitor -> IO CString

gdkMonitorGetManufacturer :: GdkMonitor -> IO (Maybe String)
gdkMonitorGetManufacturer (GdkMonitor p) = c_gdk_monitor_get_manufacturer p >>= \case
	cs	| cs == nullPtr -> pure Nothing
		| otherwise -> Just <$> peekCString cs

foreign import ccall "gdk_monitor_get_model" c_gdk_monitor_get_model ::
	Ptr GdkMonitor -> IO CString

gdkMonitorGetModel :: GdkMonitor -> IO String
gdkMonitorGetModel (GdkMonitor p) = peekCString =<< c_gdk_monitor_get_model p

foreign import ccall "gdk_monitor_get_scale_factor" c_gdk_monitor_get_scale_factor ::
	Ptr GdkMonitor -> IO #type int

gdkMonitorGetScaleFactor :: GdkMonitor -> IO #type int
gdkMonitorGetScaleFactor (GdkMonitor p) = c_gdk_monitor_get_scale_factor p

foreign import ccall "gdk_monitor_get_refresh_rate" c_gdk_monitor_get_refresh_rate ::
	Ptr GdkMonitor -> IO #type int

gdkMonitorGetRefreshRate :: GdkMonitor -> IO #type int
gdkMonitorGetRefreshRate (GdkMonitor p) = c_gdk_monitor_get_refresh_rate p

foreign import ccall "gdk_monitor_get_subpixel_layout" c_gdk_monitor_get_subpixel_layout ::
	Ptr GdkMonitor -> IO #type GdkSubpixelLayout

gdkMonitorGetSubpixelLayout :: GdkMonitor -> IO GdkSubpixelLayout
gdkMonitorGetSubpixelLayout (GdkMonitor p) = GdkSubpixelLayout <$> c_gdk_monitor_get_subpixel_layout p

foreign import ccall "gdk_monitor_is_primary" c_gdk_monitor_is_primary ::
	Ptr GdkMonitor -> IO #type gboolean

gdkMonitorIsPrimary :: GdkMonitor -> IO Bool
gdkMonitorIsPrimary (GdkMonitor p) = gbooleanToBool <$> c_gdk_monitor_is_primary p
