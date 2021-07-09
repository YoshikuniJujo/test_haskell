{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDisplay (
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
	gdkDisplayGetEvent,
	gdkDisplayPeekEvent,
	gdkDisplayPutEvent,
	gdkDisplayHasPending,

	-- * DOUBLE CLICK
	gdkDisplaySetDoubleClickTime,
	gdkDisplaySetDoubleClickDistance,

	-- * CURSOR
	gdkDisplaySupportsCursorColor,
	gdkDisplaySupportsCursorAlpha,
	gdkDisplayGetDefaultCursorSize,

	-- * GROUP
	gdkDisplayGetDefaultGroup,

	-- * SELECTION
	gdkDisplaySupportsSelectionNotification,
	gdkDisplayRequestSelectionNotification,

	-- * CLIPBOARD
	gdkDisplaySupportsClipboardPersistence,

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
import Foreign.ForeignPtr
import Foreign.C
import Control.Exception
import Data.Int
import System.IO.Unsafe
import System.GLib.Bool

import {-# SOURCE #-} Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import {-# SOURCE #-} Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom
import Graphics.Gdk.Exception
import Graphics.Gdk.Types
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
	Ptr GdkDisplay -> Ptr GdkDevice -> IO #type gboolean

gdkDisplayDeviceIsGrabbed :: GdkDisplay -> GdkDevice -> IO Bool
gdkDisplayDeviceIsGrabbed (GdkDisplay dpy) (GdkDevice pdvc) = gbooleanToBool
	<$> c_gdk_display_device_is_grabbed dpy pdvc

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

gdkDisplayGetEvent :: GdkDisplay -> IO (Maybe GdkEvent)
gdkDisplayGetEvent (GdkDisplay d) = c_gdk_display_get_event d >>= \case
	p	| p == nullPtr -> pure Nothing
		| otherwise -> Just <$> mkGdkEvent p

foreign import ccall "gdk_display_get_event" c_gdk_display_get_event ::
	Ptr GdkDisplay -> IO (Ptr GdkEvent)

gdkDisplayPeekEvent :: GdkDisplay -> IO (Maybe GdkEvent)
gdkDisplayPeekEvent (GdkDisplay d) = c_gdk_display_peek_event d >>= \case
	p	| p == nullPtr -> pure Nothing
		| otherwise -> Just <$> mkGdkEvent p

foreign import ccall "gdk_display_peek_event" c_gdk_display_peek_event ::
	Ptr GdkDisplay -> IO (Ptr GdkEvent)

gdkDisplayPutEvent :: GdkDisplay -> GdkEvent -> IO ()
gdkDisplayPutEvent (GdkDisplay d) (GdkEvent _ fe) = withForeignPtr fe \e ->
	c_gdk_display_put_event d e

foreign import ccall "gdk_display_put_event" c_gdk_display_put_event ::
	Ptr GdkDisplay -> Ptr GdkEvent -> IO ()

gdkDisplayHasPending :: GdkDisplay -> IO Bool
gdkDisplayHasPending (GdkDisplay p) = gbooleanToBool <$> c_gdk_display_has_pending p

foreign import ccall "gdk_display_has_pending" c_gdk_display_has_pending ::
	Ptr GdkDisplay -> IO #type gboolean

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

-- SELECTION

gdkDisplaySupportsSelectionNotification :: GdkDisplay -> IO Bool
gdkDisplaySupportsSelectionNotification d =
	gbooleanToBool <$> c_gdk_display_supports_selection_notification d

foreign import ccall "gdk_display_supports_selection_notification"
	c_gdk_display_supports_selection_notification ::
	GdkDisplay -> IO #{type gboolean}

gdkDisplayRequestSelectionNotification :: GdkDisplay -> GdkAtom -> IO Bool
gdkDisplayRequestSelectionNotification d s =
	gbooleanToBool <$> c_gdk_display_request_selection_notification d s

foreign import ccall "gdk_display_request_selection_notification"
	c_gdk_display_request_selection_notification ::
	GdkDisplay -> GdkAtom -> IO #{type gboolean}

-- CLIPBOARD

gdkDisplaySupportsClipboardPersistence :: GdkDisplay -> IO Bool
gdkDisplaySupportsClipboardPersistence d =
	gbooleanToBool <$> c_gdk_display_supports_clipboard_persistence d

foreign import ccall "gdk_display_supports_clipboard_persistence"
	c_gdk_display_supports_clipboard_persistence ::
	GdkDisplay -> IO #{type gboolean}

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
gdkDisplayListSeats (GdkDisplay p) = map GdkSeat <$> (g_list_to_list =<< c_gdk_display_list_seats p)

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
