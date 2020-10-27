{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkMonitor where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C

import Graphics.Gdk.Types

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
