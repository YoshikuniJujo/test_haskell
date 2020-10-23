{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDisplay where

import Foreign.Ptr
import Foreign.C
import Data.Int

import Graphics.Gdk.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_display_open" c_gdk_display_open ::
	IO (Ptr GdkDisplay)

gdkDisplayOpen :: IO GdkDisplay
gdkDisplayOpen = GdkDisplay <$> c_gdk_display_open

foreign import ccall "gdk_display_get_default" c_gdk_display_get_default ::
	IO (Ptr GdkDisplay)

gdkDisplayGetDefault :: IO GdkDisplay
gdkDisplayGetDefault = GdkDisplay <$> c_gdk_display_get_default

foreign import ccall "gdk_display_get_name" c_gdk_display_get_name ::
	Ptr GdkDisplay -> IO CString

gdkDisplayGetName :: GdkDisplay -> IO String
gdkDisplayGetName (GdkDisplay p) = peekCString =<< c_gdk_display_get_name p

foreign import ccall "gdk_display_get_default_screen" c_gdk_display_get_default_screen ::
	Ptr GdkDisplay -> IO (Ptr GdkScreen)

gdkDisplayGetDefaultScreen :: GdkDisplay -> IO GdkScreen
gdkDisplayGetDefaultScreen (GdkDisplay p) =
	GdkScreen <$> c_gdk_display_get_default_screen p

foreign import ccall "gdk_display_device_is_grabbed" c_gdk_display_device_is_grabbed ::
	Ptr GdkDisplay -> Ptr GdkDevice -> IO #type gboolean

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

gdkDisplayDeviceIsGrabbed :: GdkDisplay -> GdkDevice -> IO Bool
gdkDisplayDeviceIsGrabbed (GdkDisplay dpy) (GdkDevice dvc) = gbooleanToBool <$> c_gdk_display_device_is_grabbed dpy dvc

foreign import ccall "gdk_display_get_default_seat" c_gdk_display_get_default_seat ::
	Ptr GdkDisplay -> IO (Ptr GdkSeat)

gdkDisplayGetDefaultSeat :: GdkDisplay -> IO GdkSeat
gdkDisplayGetDefaultSeat (GdkDisplay p) = GdkSeat <$> c_gdk_display_get_default_seat p
