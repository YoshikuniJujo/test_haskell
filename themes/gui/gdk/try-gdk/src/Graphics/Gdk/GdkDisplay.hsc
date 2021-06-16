{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDisplay (
	gdkDisplayOpen,
	gdkDisplayGetDefault,
	gdkDisplayGetName,
	gdkDisplayGetDefaultScreen,
	gdkDisplayDeviceIsGrabbed,
	gdkDisplaySync,
	gdkDisplayFlush,
	gdkDisplayClose,
	gdkDisplayIsClosed,
	gdkDisplayGetEvent,
	gdkDisplayPeekEvent,
	gdkDisplayPutEvent,
	gdkDisplayHasPending,
	gdkDisplaySupportsCursorColor,
	gdkDisplaySupportsCursorAlpha,
	gdkDisplayGetDefaultCursorSize,
	gdkDisplayGetMaximalCursorSize,
	gdkDisplayGetDefaultSeat,
	gdkDisplayListSeats,
	gdkDisplayGetNMonitors,
	gdkDisplayGetMonitor,
	gdkDisplayGetPrimaryMonitor,
	gdkDisplayGetMonitorAtPoint,
	gdkDisplayGetMonitorAtWindow
	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Control.Arrow
import Control.Exception
import Data.Word
import Data.Int
import System.IO.Unsafe
import System.GLib.Bool

import Graphics.Gdk.Events
import Graphics.Gdk.Exception
import Graphics.Gdk.Types
import System.GLib.DoublyLinkedLists

#include <gdk/gdk.h>

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
gdkDisplayDeviceIsGrabbed (GdkDisplay dpy) (GdkDevice fdvc) = withForeignPtr fdvc \dvc ->
	gbooleanToBool <$> c_gdk_display_device_is_grabbed dpy dvc

foreign import ccall "gdk_display_sync" c_gdk_display_sync ::
	Ptr GdkDisplay -> IO ()

gdkDisplaySync :: GdkDisplay -> IO ()
gdkDisplaySync (GdkDisplay p) = c_gdk_display_sync p

foreign import ccall "gdk_display_flush" c_gdk_display_flush ::
	Ptr GdkDisplay -> IO ()

gdkDisplayFlush :: GdkDisplay -> IO ()
gdkDisplayFlush (GdkDisplay p) = c_gdk_display_flush p

foreign import ccall "gdk_display_close" c_gdk_display_close ::
	Ptr GdkDisplay -> IO ()

gdkDisplayClose :: GdkDisplay -> IO ()
gdkDisplayClose (GdkDisplay p) = c_gdk_display_close p

foreign import ccall "gdk_display_is_closed" c_gdk_display_is_closed ::
	Ptr GdkDisplay -> IO #type gboolean

gdkDisplayIsClosed :: GdkDisplay -> IO Bool
gdkDisplayIsClosed (GdkDisplay p) = gbooleanToBool <$> c_gdk_display_is_closed p

foreign import ccall "gdk_display_get_event" c_gdk_display_get_event ::
	Ptr GdkDisplay -> IO (Ptr GdkEvent)

gdkDisplayGetEvent :: GdkDisplay -> IO (Maybe GdkEvent)
gdkDisplayGetEvent (GdkDisplay d) = c_gdk_display_get_event d >>= \case
	p	| p == nullPtr -> pure Nothing
		| otherwise -> Just <$> mkGdkEvent p

foreign import ccall "gdk_display_peek_event" c_gdk_display_peek_event ::
	Ptr GdkDisplay -> IO (Ptr GdkEvent)

gdkDisplayPeekEvent :: GdkDisplay -> IO (Maybe GdkEvent)
gdkDisplayPeekEvent (GdkDisplay d) = c_gdk_display_peek_event d >>= \case
	p	| p == nullPtr -> pure Nothing
		| otherwise -> Just <$> mkGdkEvent p

foreign import ccall "gdk_display_put_event" c_gdk_display_put_event ::
	Ptr GdkDisplay -> Ptr GdkEvent -> IO ()

gdkDisplayPutEvent :: GdkDisplay -> GdkEvent -> IO ()
gdkDisplayPutEvent (GdkDisplay d) (GdkEvent _ fe) = withForeignPtr fe \e ->
	c_gdk_display_put_event d e

foreign import ccall "gdk_display_has_pending" c_gdk_display_has_pending ::
	Ptr GdkDisplay -> IO #type gboolean

gdkDisplayHasPending :: GdkDisplay -> IO Bool
gdkDisplayHasPending (GdkDisplay p) = gbooleanToBool <$> c_gdk_display_has_pending p

foreign import ccall "gdk_display_supports_cursor_color" c_gdk_display_supports_cursor_color ::
	Ptr GdkDisplay -> IO #type gboolean

gdkDisplaySupportsCursorColor :: GdkDisplay -> IO Bool
gdkDisplaySupportsCursorColor (GdkDisplay p) =
	gbooleanToBool <$> c_gdk_display_supports_cursor_color p

foreign import ccall "gdk_display_supports_cursor_alpha" c_gdk_display_supports_cursor_alpha ::
	Ptr GdkDisplay -> IO #type gboolean

gdkDisplaySupportsCursorAlpha :: GdkDisplay -> IO Bool
gdkDisplaySupportsCursorAlpha (GdkDisplay p) =
	gbooleanToBool <$> c_gdk_display_supports_cursor_alpha p

foreign import ccall "gdk_display_get_default_cursor_size" c_gdk_display_get_default_cursor_size ::
	Ptr GdkDisplay -> IO #type guint

gdkDisplayGetDefaultCursorSize :: GdkDisplay -> IO #type guint
gdkDisplayGetDefaultCursorSize (GdkDisplay p) = c_gdk_display_get_default_cursor_size p

foreign import ccall "gdk_display_get_maximal_cursor_size" c_gdk_display_get_maximal_cursor_size ::
	Ptr GdkDisplay -> Ptr #{type guint} -> Ptr #{type guint} -> IO ()

gdkDisplayGetMaximalCursorSize :: GdkDisplay -> IO (#{type guint}, #type guint)
gdkDisplayGetMaximalCursorSize (GdkDisplay d) = alloca \x -> alloca \y -> do
	c_gdk_display_get_maximal_cursor_size d x y
	(,) <$> peek x <*> peek y

foreign import ccall "gdk_display_get_default_seat" c_gdk_display_get_default_seat ::
	Ptr GdkDisplay -> IO (Ptr GdkSeat)

gdkDisplayGetDefaultSeat :: GdkDisplay -> IO GdkSeat
gdkDisplayGetDefaultSeat (GdkDisplay p) = GdkSeat <$> c_gdk_display_get_default_seat p

foreign import ccall "gdk_display_list_seats" c_gdk_display_list_seats ::
	Ptr GdkDisplay -> IO (Ptr (GList GdkSeat))

gdkDisplayListSeats :: GdkDisplay -> IO ([GdkSeat], [GdkSeat])
gdkDisplayListSeats (GdkDisplay p) = (map GdkSeat *** map GdkSeat) <$> (gListListPtr =<< GListRef <$> c_gdk_display_list_seats p)

foreign import ccall "gdk_display_get_n_monitors" c_gdk_display_get_n_monitors ::
	Ptr GdkDisplay -> IO #type int

gdkDisplayGetNMonitors :: GdkDisplay -> IO #type int
gdkDisplayGetNMonitors (GdkDisplay p) = c_gdk_display_get_n_monitors p

foreign import ccall "gdk_display_get_monitor" c_gdk_display_get_monitor ::
	Ptr GdkDisplay -> #{type int} -> IO (Ptr GdkMonitor)

gdkDisplayGetMonitor :: GdkDisplay -> #{type int} -> IO GdkMonitor
gdkDisplayGetMonitor (GdkDisplay p) n = GdkMonitor <$> c_gdk_display_get_monitor p n

foreign import ccall "gdk_display_get_primary_monitor" c_gdk_display_get_primary_monitor ::
	Ptr GdkDisplay -> IO (Ptr GdkMonitor)

gdkDisplayGetPrimaryMonitor :: GdkDisplay -> IO (Maybe GdkMonitor)
gdkDisplayGetPrimaryMonitor (GdkDisplay p)
	| p == nullPtr = pure Nothing
	| otherwise = Just . GdkMonitor <$> c_gdk_display_get_primary_monitor p

foreign import ccall "gdk_display_get_monitor_at_point" c_gdk_display_get_monitor_at_point ::
	Ptr GdkDisplay -> #{type int} -> #{type int} -> IO (Ptr GdkMonitor)

gdkDisplayGetMonitorAtPoint :: GdkDisplay -> #{type int} -> #{type int} -> IO GdkMonitor
gdkDisplayGetMonitorAtPoint (GdkDisplay p) x y = GdkMonitor <$> c_gdk_display_get_monitor_at_point p x y

foreign import ccall "gdk_display_get_monitor_at_window" c_gdk_display_get_monitor_at_window ::
	Ptr GdkDisplay -> Ptr GdkWindow -> IO (Ptr GdkMonitor)

gdkDisplayGetMonitorAtWindow :: GdkDisplay -> GdkWindow -> IO GdkMonitor
gdkDisplayGetMonitorAtWindow (GdkDisplay d) (GdkWindow w) = GdkMonitor <$> c_gdk_display_get_monitor_at_window d w
