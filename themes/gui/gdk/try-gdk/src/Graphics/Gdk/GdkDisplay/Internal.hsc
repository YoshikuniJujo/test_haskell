{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDisplay.Internal (
	-- * TYPE
	GdkDisplay(..),

	-- * DISPLAY
	gdkDisplayOpen,
	gdkDisplayGetDefault,
	gdkDisplayGetName,
	gdkDisplayGetDefaultScreen,
	gdkDisplayDeviceIsGrabbed,
	gdkDisplaySync,
	gdkDisplayFlush,
	gdkDisplayClose,
	gdkDisplayIsClosed,

	-- * EVENT
	gdkDisplayWithEvent,

	-- * DOUBLE CLICK
	gdkDisplaySetDoubleClickTime,
	gdkDisplaySetDoubleClickDistance,

	-- * CURSOR
	gdkDisplaySupportsCursorColor,
	gdkDisplaySupportsCursorAlpha,
	gdkDisplayGetDefaultCursorSize,

	-- * GROUP
	gdkDisplayGetDefaultGroup,

	-- * SEAT
	gdkDisplayGetDefaultSeat,
	gdkDisplayListSeats,

	-- * MONITOR
	gdkDisplayGetNMonitors,
	gdkDisplayGetMonitor,
	gdkDisplayGetPrimaryMonitor,
	gdkDisplayGetMonitorAtPoint,
	gdkDisplayGetMonitorAtWindow ) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.Concurrent
import Foreign.C
import Control.Exception
import Data.Int
import System.IO.Unsafe
import System.GLib.Bool

import {-# SOURCE #-} Graphics.Gdk.GdkScreen.Internal
import Graphics.Gdk.GdkSeat.Internal
import Graphics.Gdk.GdkMonitor.Internal
import Graphics.Gdk.GdkDevice.Internal
import {-# SOURCE #-} Graphics.Gdk.Windows.Internal
import Graphics.Gdk.EventStructures.Internal
import Graphics.Gdk.Exception
import System.GLib.DoublyLinkedLists

#include <gdk/gdk.h>

newtype GdkDisplay = GdkDisplay (Ptr GdkDisplay) deriving Show

gdkDisplayOpen :: String -> IO GdkDisplay
gdkDisplayOpen dn = (GdkDisplay <$>)
	$ withCString dn c_gdk_display_open >>= \case
		NullPtr -> throw GdkCannotOpenDisplay; pd -> pure pd

foreign import ccall "gdk_display_open" c_gdk_display_open ::
	CString -> IO (Ptr GdkDisplay)

gdkDisplayGetDefault :: IO GdkDisplay
gdkDisplayGetDefault = (GdkDisplay <$>)
	$ c_gdk_display_get_default >>= \case
		NullPtr -> throw GdkNoDefaultDisplay; pd -> pure pd

foreign import ccall "gdk_display_get_default" c_gdk_display_get_default ::
	IO (Ptr GdkDisplay)

foreign import ccall "gdk_display_get_name" c_gdk_display_get_name ::
	Ptr GdkDisplay -> IO CString

gdkDisplayGetName :: GdkDisplay -> String
gdkDisplayGetName (GdkDisplay p) =
	unsafePerformIO $ peekCString =<< c_gdk_display_get_name p

foreign import ccall "gdk_display_get_default_screen" c_gdk_display_get_default_screen ::
	Ptr GdkDisplay -> IO (Ptr GdkScreen)

gdkDisplayGetDefaultScreen :: GdkDisplay -> GdkScreen
gdkDisplayGetDefaultScreen (GdkDisplay p) =
	unsafePerformIO $ GdkScreen <$> c_gdk_display_get_default_screen p

foreign import ccall "gdk_display_device_is_grabbed" c_gdk_display_device_is_grabbed ::
	GdkDisplay -> GdkDevice -> IO #type gboolean

gdkDisplayDeviceIsGrabbed :: IsGdkDevice d => GdkDisplay -> d pk -> IO Bool
gdkDisplayDeviceIsGrabbed dpy dvc = gbooleanToBool
	<$> c_gdk_display_device_is_grabbed dpy (getGdkDevice dvc)

gdkDisplaySync :: GdkDisplay -> IO ()
gdkDisplaySync (GdkDisplay p) = c_gdk_display_sync p

foreign import ccall "gdk_display_sync" c_gdk_display_sync ::
	Ptr GdkDisplay -> IO ()

gdkDisplayFlush :: GdkDisplay -> IO ()
gdkDisplayFlush (GdkDisplay p) = c_gdk_display_flush p

foreign import ccall "gdk_display_flush" c_gdk_display_flush ::
	Ptr GdkDisplay -> IO ()

gdkDisplayClose :: GdkDisplay -> IO ()
gdkDisplayClose (GdkDisplay p) = c_gdk_display_close p

foreign import ccall "gdk_display_close" c_gdk_display_close ::
	Ptr GdkDisplay -> IO ()

gdkDisplayIsClosed :: GdkDisplay -> IO Bool
gdkDisplayIsClosed (GdkDisplay p) = gbooleanToBool <$> c_gdk_display_is_closed p

foreign import ccall "gdk_display_is_closed" c_gdk_display_is_closed ::
	Ptr GdkDisplay -> IO #type gboolean

-- EVENT

gdkDisplayWithEvent :: GdkDisplay -> (forall s . Maybe (GdkEvent s) -> IO a) -> IO a
gdkDisplayWithEvent d f = c_gdk_display_get_event d >>= \case
	NullPtr -> f Nothing
	p -> (f . Just . GdkEvent =<< newForeignPtr p (pure ()))
		<* c_gdk_event_free p

foreign import ccall "gdk_display_get_event"
	c_gdk_display_get_event :: GdkDisplay -> IO (Ptr GdkEventTag)

-- DOUBLE CLICK

gdkDisplaySetDoubleClickTime :: GdkDisplay -> CUInt -> IO ()
gdkDisplaySetDoubleClickTime (GdkDisplay d) =
	c_gdk_display_set_double_click_time d

foreign import ccall "gdk_display_set_double_click_time"
	c_gdk_display_set_double_click_time :: Ptr GdkDisplay -> CUInt -> IO ()

gdkDisplaySetDoubleClickDistance :: GdkDisplay -> CUInt -> IO ()
gdkDisplaySetDoubleClickDistance (GdkDisplay d) =
	c_gdk_display_set_double_click_distance d

foreign import ccall "gdk_display_set_double_click_distance"
	c_gdk_display_set_double_click_distance ::
	Ptr GdkDisplay -> CUInt -> IO ()

-- GROUP

foreign import ccall "gdk_display_get_default_group"
	gdkDisplayGetDefaultGroup :: GdkDisplay -> IO GdkWindow

-- CURSOR

gdkDisplaySupportsCursorColor :: GdkDisplay -> IO Bool
gdkDisplaySupportsCursorColor (GdkDisplay p) =
	gbooleanToBool <$> c_gdk_display_supports_cursor_color p

foreign import ccall "gdk_display_supports_cursor_color" c_gdk_display_supports_cursor_color ::
	Ptr GdkDisplay -> IO #type gboolean

gdkDisplaySupportsCursorAlpha :: GdkDisplay -> IO Bool
gdkDisplaySupportsCursorAlpha (GdkDisplay p) =
	gbooleanToBool <$> c_gdk_display_supports_cursor_alpha p

foreign import ccall "gdk_display_supports_cursor_alpha" c_gdk_display_supports_cursor_alpha ::
	Ptr GdkDisplay -> IO #type gboolean

gdkDisplayGetDefaultCursorSize :: GdkDisplay -> IO CUInt
gdkDisplayGetDefaultCursorSize (GdkDisplay p) = c_gdk_display_get_default_cursor_size p

foreign import ccall "gdk_display_get_default_cursor_size" c_gdk_display_get_default_cursor_size ::
	Ptr GdkDisplay -> IO CUInt

gdkDisplayGetDefaultSeat :: GdkDisplay -> IO GdkSeat
gdkDisplayGetDefaultSeat (GdkDisplay p) = GdkSeat <$> c_gdk_display_get_default_seat p

foreign import ccall "gdk_display_get_default_seat" c_gdk_display_get_default_seat ::
	Ptr GdkDisplay -> IO (Ptr GdkSeat)

gdkDisplayListSeats :: GdkDisplay -> IO [GdkSeat]
gdkDisplayListSeats (GdkDisplay p) = maybe [] (map GdkSeat) <$> (g_list_to_list =<< c_gdk_display_list_seats p)

foreign import ccall "gdk_display_list_seats" c_gdk_display_list_seats ::
	Ptr GdkDisplay -> IO (Ptr (GList GdkSeat))

gdkDisplayGetNMonitors :: GdkDisplay -> IO #type int
gdkDisplayGetNMonitors (GdkDisplay p) = c_gdk_display_get_n_monitors p

foreign import ccall "gdk_display_get_n_monitors" c_gdk_display_get_n_monitors ::
	Ptr GdkDisplay -> IO #type int

gdkDisplayGetMonitor :: GdkDisplay -> #{type int} -> IO GdkMonitor
gdkDisplayGetMonitor (GdkDisplay pd) n = (GdkMonitor <$>)
	$ c_gdk_display_get_monitor pd n >>= \case
		NullPtr -> throw GdkIndexOutOfRange; pm -> pure pm

foreign import ccall "gdk_display_get_monitor" c_gdk_display_get_monitor ::
	Ptr GdkDisplay -> #{type int} -> IO (Ptr GdkMonitor)

gdkDisplayGetPrimaryMonitor :: GdkDisplay -> IO (Maybe GdkMonitor)
gdkDisplayGetPrimaryMonitor (GdkDisplay p)
	| p == nullPtr = pure Nothing
	| otherwise = Just . GdkMonitor <$> c_gdk_display_get_primary_monitor p

foreign import ccall "gdk_display_get_primary_monitor" c_gdk_display_get_primary_monitor ::
	Ptr GdkDisplay -> IO (Ptr GdkMonitor)

gdkDisplayGetMonitorAtPoint :: GdkDisplay -> CInt -> CInt -> IO GdkMonitor
gdkDisplayGetMonitorAtPoint (GdkDisplay p) x y = GdkMonitor <$> c_gdk_display_get_monitor_at_point p x y

foreign import ccall "gdk_display_get_monitor_at_point" c_gdk_display_get_monitor_at_point ::
	Ptr GdkDisplay -> CInt -> CInt -> IO (Ptr GdkMonitor)

gdkDisplayGetMonitorAtWindow :: GdkDisplay -> GdkWindow -> IO GdkMonitor
gdkDisplayGetMonitorAtWindow (GdkDisplay d) (GdkWindow w) = GdkMonitor <$> c_gdk_display_get_monitor_at_window d w

foreign import ccall "gdk_display_get_monitor_at_window" c_gdk_display_get_monitor_at_window ::
	Ptr GdkDisplay -> Ptr GdkWindow -> IO (Ptr GdkMonitor)
