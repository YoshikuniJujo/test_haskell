{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkSeat where

import Foreign.Ptr

import Graphics.Gdk.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_seat_get_pointer" c_gdk_seat_get_pointer ::
	Ptr GdkSeat -> IO (Ptr GdkDevice)

gdkSeatGetPointer :: GdkSeat -> IO GdkDevice
gdkSeatGetPointer (GdkSeat p) = GdkDevice <$> c_gdk_seat_get_pointer p

foreign import ccall "gdk_seat_get_keyboard" c_gdk_seat_get_keyboard ::
	Ptr GdkSeat -> IO (Ptr GdkDevice)

gdkSeatGetKeyboard :: GdkSeat -> IO GdkDevice
gdkSeatGetKeyboard (GdkSeat p) = GdkDevice <$> c_gdk_seat_get_keyboard p
