{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkSeat where

import Foreign.Ptr
import Data.Word

import Graphics.Gdk.Types
import Graphics.Gdk.Values

#include <gdk/gdk.h>

foreign import ccall "gdk_seat_get_display" c_gdk_seat_get_display ::
	Ptr GdkSeat -> IO (Ptr GdkDisplay)

gdkSeatGetDisplay :: GdkSeat -> IO GdkDisplay
gdkSeatGetDisplay (GdkSeat p) = GdkDisplay <$> c_gdk_seat_get_display p

foreign import ccall "gdk_seat_get_pointer" c_gdk_seat_get_pointer ::
	Ptr GdkSeat -> IO (Ptr GdkDevice)

gdkSeatGetPointer :: GdkSeat -> IO GdkDevice
gdkSeatGetPointer (GdkSeat p) = GdkDevice <$> c_gdk_seat_get_pointer p

foreign import ccall "gdk_seat_get_keyboard" c_gdk_seat_get_keyboard ::
	Ptr GdkSeat -> IO (Ptr GdkDevice)

gdkSeatGetKeyboard :: GdkSeat -> IO GdkDevice
gdkSeatGetKeyboard (GdkSeat p) = GdkDevice <$> c_gdk_seat_get_keyboard p

foreign import ccall "gdk_seat_get_capabilities" c_gdk_seat_get_capabilities ::
	Ptr GdkSeat -> IO #type GdkSeatCapabilities

gdkSeatGetCapabilities :: GdkSeat -> IO GdkSeatCapabilities
gdkSeatGetCapabilities (GdkSeat p) = GdkSeatCapabilities <$> c_gdk_seat_get_capabilities p
