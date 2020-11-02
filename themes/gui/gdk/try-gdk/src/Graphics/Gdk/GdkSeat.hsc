{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkSeat where

import Foreign.Ptr
import Control.Arrow
import Data.Word
import Data.Int

import Graphics.Gdk.Events
import Graphics.Gdk.Types
import Graphics.Gdk.Values

import System.Glib.DoublyLinkedLists

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

foreign import ccall "gdk_seat_get_slaves" c_gdk_seat_get_slaves ::
	Ptr GdkSeat -> #{type GdkSeatCapabilities} -> IO (Ptr (GList GdkDevice))

gdkSeatGetSlaves :: GdkSeat -> GdkSeatCapabilities -> IO ([GdkDevice], [GdkDevice])
gdkSeatGetSlaves (GdkSeat p) (GdkSeatCapabilities cps) = do
	gl <- c_gdk_seat_get_slaves p cps
	(map GdkDevice *** map GdkDevice) <$> gListListPtr (GListRef gl)
		<* c_g_list_free gl

foreign import ccall "g_list_free" c_g_list_free :: Ptr (GList a) -> IO ()

foreign import ccall "gdk_seat_grab" c_gdk_seat_grab ::
	Ptr GdkSeat -> Ptr GdkWindow -> #{type GdkSeatCapabilities} -> #{type gboolean} ->
	Ptr GdkCursor -> Ptr GdkEvent -> Ptr () -> Ptr a -> IO #{type GdkGrabStatus}

gdkSeatGrabSimple :: GdkSeat -> GdkWindow -> IO #{type GdkGrabStatus}
gdkSeatGrabSimple (GdkSeat st) (GdkWindow wn) =
--	c_gdk_seat_grab st wn #{const GDK_SEAT_CAPABILITY_ALL_POINTING} #{const TRUE} nullPtr nullPtr nullPtr nullPtr
	c_gdk_seat_grab st wn #{const GDK_SEAT_CAPABILITY_ALL_POINTING} #{const FALSE} nullPtr nullPtr nullPtr nullPtr
