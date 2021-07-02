{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows (
	-- * TYPE
	GdkWindow(..), withGdkWindowAutoUnref,

	-- * Checked
	gdkWindowNew, gdkWindowDestroy, gdkWindowGetWindowType,
	gdkWindowGetDisplay, gdkWindowGetScreen, gdkWindowGetVisual,
	gdkWindowShow, gdkWindowShowUnraised, gdkWindowHide,
	gdkWindowIsDestroyed, gdkWindowIsVisible, gdkWindowIsViewable,
	gdkWindowIsInputOnly, gdkWindowIsShaped, gdkWindowGetState,
	gdkWindowWithdraw,
	gdkWindowIconify, gdkWindowDeiconify, gdkWindowStick, gdkWindowUnstick,
	gdkWindowMaximize, gdkWindowUnmaximize,
	gdkWindowFullscreen, gdkWindowUnfullscreen,
	gdkWindowGetFullscreenMode, gdkWindowSetFullscreenMode,
	gdkWindowSetKeepAbove, gdkWindowSetKeepBelow,
	gdkWindowSetOpacity,
	gdkWindowSetPassThrough, gdkWindowGetPassThrough,
	gdkWindowMove, gdkWindowResize, gdkWindowMoveResize,
	gdkWindowReparent,

	-- * Not Checked
	gdkWindowFreezeUpdates, gdkWindowThawUpdates,
	gdkWindowWithDrawFrame, gdkWindowInvalidateRect, gdkWindowSetEvents,

	gdkWindowSetTitle, c_gdk_window_set_title, gdkWindowSetCursor, gdkWindowGetCursor,
	gdkWindowGetWidth, gdkWindowGetHeight, gdkWindowGetPosition,

	gdkWindowGetParent, gdkWindowGetDecorations, gdkGetDefaultRootWindow,
	gdkWindowSetDeviceCursor, gdkWindowSetDeviceEvents, gdkWindowSetSourceEvents,

	gdkWindowSetEventCompression,

	GdkWindowAttr(..), minimalGdkWindowAttr,

	-- * GdkWindowType
	GdkWindowType(..),
	pattern GdkWindowRoot, pattern GdkWindowToplevel,
	pattern GdkWindowChild, pattern GdkWindowTemp, pattern GdkWindowForeign,
	pattern GdkWindowOffscreen, pattern GdkWindowSubsurface ) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Foreign.C.Enum
import Control.Exception
import Data.Word
import Data.Int
import System.GLib.Bool

import {-# SOURCE #-} Graphics.Gdk.GdkDisplay
import {-# SOURCE #-} Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.PointsAndRectangles
import Graphics.Gdk.Visuals
import Graphics.Gdk.Cursors
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Types
import Graphics.Gdk.Values
import Graphics.Gdk.EventStructures

import Graphics.Cairo.Types

#include <gdk/gdk.h>

newtype GdkWindow = GdkWindow (Ptr GdkWindow) deriving Show

withGdkWindowAutoUnref :: GdkWindowAutoUnref -> (GdkWindow -> IO a) -> IO a
withGdkWindowAutoUnref (GdkWindowAutoUnref fwnu) f =
	withForeignPtr fwnu \pwnu -> f (GdkWindow $ castPtr pwnu)

gdkWindowNew :: Maybe GdkWindow -> GdkWindowAttr -> IO GdkWindow
gdkWindowNew mw a = maybe ($ nullPtr) (\(GdkWindow pw) -> ($ pw)) mw \pw -> do
	whenMaybe cursorRef $ gdkWindowAttrCursor a
	withGdkWindowAttr a \pa ts -> c_gdk_window_new pw pa ts
	where cursorRef (GdkCursor fc) = withForeignPtr fc c_g_object_ref

foreign import ccall "gdk_window_new" c_gdk_window_new ::
	Ptr GdkWindow -> Ptr GdkWindowAttr -> GdkWindowAttributesTypes ->
	IO GdkWindow

foreign import ccall "g_object_ref" c_g_object_ref :: Ptr a -> IO ()

gdkWindowDestroy :: GdkWindow -> IO ()
gdkWindowDestroy w = do
	whenMaybe c_g_object_unref =<< c_gdk_window_get_cursor' w
	c_gdk_window_destroy w

foreign import ccall "gdk_window_destroy"
	c_gdk_window_destroy :: GdkWindow -> IO ()

c_gdk_window_get_cursor' :: GdkWindow -> IO (Maybe (Ptr GdkCursor))
c_gdk_window_get_cursor' w = (<$> c_gdk_window_get_cursor w) \case
	NullPtr -> Nothing; p -> Just p

foreign import ccall "gdk_window_get_cursor" c_gdk_window_get_cursor :: GdkWindow -> IO (Ptr GdkCursor)

foreign import ccall "gdk_window_get_window_type"
	gdkWindowGetWindowType :: GdkWindow -> IO GdkWindowType

foreign import ccall "gdk_window_get_display"
	gdkWindowGetDisplay :: GdkWindow -> IO GdkDisplay

foreign import ccall "gdk_window_get_screen"
	gdkWindowGetScreen :: GdkWindow -> IO GdkScreen

foreign import ccall "gdk_window_get_visual"
	gdkWindowGetVisual :: GdkWindow -> IO GdkVisual

foreign import ccall "gdk_window_show"
	gdkWindowShow :: GdkWindow -> IO ()

foreign import ccall "gdk_window_show_unraised"
	gdkWindowShowUnraised :: GdkWindow -> IO ()

foreign import ccall "gdk_window_hide"
	gdkWindowHide :: GdkWindow -> IO ()

gdkWindowIsDestroyed :: GdkWindow -> IO Bool
gdkWindowIsDestroyed w = gbooleanToBool <$> c_gdk_window_is_destroyed w

foreign import ccall "gdk_window_is_destroyed"
	c_gdk_window_is_destroyed :: GdkWindow -> IO #type gboolean

gdkWindowIsVisible :: GdkWindow -> IO Bool
gdkWindowIsVisible w = gbooleanToBool <$> c_gdk_window_is_visible w

foreign import ccall "gdk_window_is_visible"
	c_gdk_window_is_visible :: GdkWindow -> IO #type gboolean

gdkWindowIsViewable :: GdkWindow -> IO Bool
gdkWindowIsViewable w = gbooleanToBool <$> c_gdk_window_is_viewable w

foreign import ccall "gdk_window_is_viewable"
	c_gdk_window_is_viewable :: GdkWindow -> IO #type gboolean

gdkWindowIsInputOnly :: GdkWindow -> IO Bool
gdkWindowIsInputOnly w = gbooleanToBool <$> c_gdk_window_is_input_only w

foreign import ccall "gdk_window_is_input_only"
	c_gdk_window_is_input_only :: GdkWindow -> IO #type gboolean

gdkWindowIsShaped :: GdkWindow -> IO Bool
gdkWindowIsShaped w = gbooleanToBool <$> c_gdk_window_is_shaped w

foreign import ccall "gdk_window_is_shaped"
	c_gdk_window_is_shaped :: GdkWindow -> IO #type gboolean

gdkWindowGetState :: GdkWindow -> IO GdkWindowStates
gdkWindowGetState w = GdkWindowStates <$> c_gdk_window_get_state w

foreign import ccall "gdk_window_get_state"
	c_gdk_window_get_state :: GdkWindow -> IO #type GdkWindowState

foreign import ccall "gdk_window_withdraw"
	gdkWindowWithdraw :: GdkWindow -> IO ()

foreign import ccall "gdk_window_iconify" gdkWindowIconify :: GdkWindow -> IO ()

foreign import ccall "gdk_window_deiconify"
	gdkWindowDeiconify :: GdkWindow -> IO ()

foreign import ccall "gdk_window_stick" gdkWindowStick :: GdkWindow -> IO ()
foreign import ccall "gdk_window_unstick" gdkWindowUnstick :: GdkWindow -> IO ()

foreign import ccall "gdk_window_maximize"
	gdkWindowMaximize :: GdkWindow -> IO ()

foreign import ccall "gdk_window_unmaximize"
	gdkWindowUnmaximize :: GdkWindow -> IO ()

foreign import ccall "gdk_window_fullscreen"
	gdkWindowFullscreen :: GdkWindow -> IO ()

foreign import ccall "gdk_window_unfullscreen"
	gdkWindowUnfullscreen :: GdkWindow -> IO ()

enum "GdkFullscreenMode" ''#{type GdkFullscreenMode} [''Show] [
	("GdkFullscreenOnCurrentMonitor",
		#{const GDK_FULLSCREEN_ON_CURRENT_MONITOR}),
	("GdkFullscreenOnAllMonitors",
		#{const GDK_FULLSCREEN_ON_ALL_MONITORS}) ]

foreign import ccall "gdk_window_get_fullscreen_mode"
	gdkWindowGetFullscreenMode :: GdkWindow -> IO GdkFullscreenMode

foreign import ccall "gdk_window_set_fullscreen_mode"
	gdkWindowSetFullscreenMode :: GdkWindow -> GdkFullscreenMode -> IO ()

gdkWindowSetKeepAbove :: GdkWindow -> Bool -> IO ()
gdkWindowSetKeepAbove w = c_gdk_window_set_keep_above w . boolToGboolean

foreign import ccall "gdk_window_set_keep_above"
	c_gdk_window_set_keep_above :: GdkWindow -> #{type gboolean} -> IO ()

gdkWindowSetKeepBelow :: GdkWindow -> Bool -> IO ()
gdkWindowSetKeepBelow w = c_gdk_window_set_keep_below w . boolToGboolean

foreign import ccall "gdk_window_set_keep_below"
	c_gdk_window_set_keep_below :: GdkWindow -> #{type gboolean} -> IO ()

foreign import ccall "gdk_window_set_opacity"
	gdkWindowSetOpacity :: GdkWindow -> CDouble -> IO ()

gdkWindowSetPassThrough :: GdkWindow -> Bool -> IO ()
gdkWindowSetPassThrough w = c_gdk_window_set_pass_through w . boolToGboolean

foreign import ccall "gdk_window_set_pass_through"
	c_gdk_window_set_pass_through :: GdkWindow -> #{type gboolean} -> IO ()

gdkWindowGetPassThrough :: GdkWindow -> IO Bool
gdkWindowGetPassThrough w = gbooleanToBool <$> c_gdk_window_get_pass_through w

foreign import ccall "gdk_window_get_pass_through"
	c_gdk_window_get_pass_through :: GdkWindow -> IO #{type gboolean}

foreign import ccall "gdk_window_move"
	gdkWindowMove :: GdkWindow -> CInt -> CInt -> IO ()

foreign import ccall "gdk_window_resize"
	gdkWindowResize :: GdkWindow -> CInt -> CInt -> IO ()

foreign import ccall "gdk_window_move_resize"
	gdkWindowMoveResize ::
	GdkWindow -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "gdk_window_reparent"
	gdkWindowReparent :: GdkWindow -> GdkWindow -> CInt -> CInt -> IO ()

foreign import ccall "gdk_window_begin_draw_frame" c_gdk_window_begin_draw_frame ::
	Ptr GdkWindow -> Ptr (CairoRegionT s) -> IO (Ptr GdkDrawingContext)

foreign import ccall "gdk_window_end_draw_frame" c_gdk_window_end_draw_frame ::
	Ptr GdkWindow -> Ptr GdkDrawingContext -> IO ()

gdkWindowWithDrawFrame :: GdkWindow -> CairoRegionT s -> (GdkDrawingContext -> IO a) -> IO a
gdkWindowWithDrawFrame (GdkWindow w) (CairoRegionT fr) act = withForeignPtr fr \r -> bracket
	(c_gdk_window_begin_draw_frame w r) (c_gdk_window_end_draw_frame w) $ (. GdkDrawingContext) act

foreign import ccall "gdk_window_invalidate_rect" c_gdk_window_invalidate_rect ::
	Ptr GdkWindow -> Ptr GdkRectangle -> #{type gboolean} -> IO ()

gdkWindowInvalidateRect :: GdkWindow -> (#{type int}, #{type int}) -> (#{type int}, #{type int}) -> Bool -> IO ()
gdkWindowInvalidateRect (GdkWindow win) (x, y) (w, h) b = allocaBytes #{size GdkRectangle} \p -> do
	#{poke GdkRectangle, x} p x
	#{poke GdkRectangle, y} p y
	#{poke GdkRectangle, width} p w
	#{poke GdkRectangle, height} p h
	c_gdk_window_invalidate_rect win p $ boolToGboolean b

foreign import ccall "gdk_window_freeze_updates" c_gdk_window_freeze_updates :: Ptr GdkWindow -> IO ()

gdkWindowFreezeUpdates :: GdkWindow -> IO ()
gdkWindowFreezeUpdates (GdkWindow p) = c_gdk_window_freeze_updates p

foreign import ccall "gdk_window_thaw_updates" c_gdk_window_thaw_updates :: Ptr GdkWindow -> IO ()

gdkWindowThawUpdates :: GdkWindow -> IO ()
gdkWindowThawUpdates (GdkWindow p) = c_gdk_window_thaw_updates p

foreign import ccall "gdk_window_set_events" c_gdk_window_set_events :: Ptr GdkWindow -> #{type GdkEventMask} -> IO ()

gdkWindowSetEvents :: GdkWindow -> [GdkEventMask] -> IO ()
gdkWindowSetEvents (GdkWindow p) m = c_gdk_window_set_events p (mergeGdkEventMask m)

gdkWindowSetTitle :: GdkWindow -> String -> IO ()
gdkWindowSetTitle w ttl = withCString ttl \cttl -> c_gdk_window_set_title w cttl

foreign import ccall "gdk_window_set_title" c_gdk_window_set_title :: GdkWindow -> CString -> IO ()

gdkWindowSetCursor :: GdkWindow -> GdkCursor -> IO ()
gdkWindowSetCursor w (GdkCursor fc) = do
	po <- c_gdk_window_get_cursor' w
	whenMaybe c_g_object_unref po
	withForeignPtr fc \c -> do
		c_g_object_ref c
		c_gdk_window_set_cursor w c

foreign import ccall "gdk_window_set_cursor" c_gdk_window_set_cursor :: GdkWindow -> Ptr GdkCursor -> IO ()

gdkWindowGetCursor :: GdkWindow -> IO (Maybe GdkCursor)
gdkWindowGetCursor w = do
	mc <- c_gdk_window_get_cursor' w
	whenMaybe c_g_object_ref mc
	case mc of
		Nothing -> pure Nothing
		Just c -> Just . GdkCursor <$> newForeignPtr c (c_g_object_unref c)

foreign import ccall "gdk_window_get_width" c_gdk_window_get_width :: Ptr GdkWindow -> IO #type int

gdkWindowGetWidth :: GdkWindow -> IO #type int
gdkWindowGetWidth (GdkWindow w) = c_gdk_window_get_width w

foreign import ccall "gdk_window_get_height" c_gdk_window_get_height :: Ptr GdkWindow -> IO #type int

gdkWindowGetHeight :: GdkWindow -> IO #type int
gdkWindowGetHeight (GdkWindow w) = c_gdk_window_get_height w

foreign import ccall "gdk_window_get_position" c_gdk_window_get_position ::
	Ptr GdkWindow -> Ptr #{type gint} -> Ptr #{type gint} -> IO ()

gdkWindowGetPosition :: GdkWindow -> IO (#{type gint}, #{type gint})
gdkWindowGetPosition (GdkWindow p) = alloca \x -> alloca \y -> do
	c_gdk_window_get_position p x y
	(,) <$> peek x <*> peek y

foreign import ccall "gdk_window_get_parent" c_gdk_window_get_parent ::
	Ptr GdkWindow -> IO (Ptr GdkWindow)

gdkWindowGetParent :: GdkWindow -> IO GdkWindow
gdkWindowGetParent (GdkWindow p) = GdkWindow <$> c_gdk_window_get_parent p

foreign import ccall "gdk_window_get_decorations" c_gdk_window_get_decorations ::
	Ptr GdkWindow -> IO #type GdkWMDecoration

gdkWindowGetDecorations :: GdkWindow -> IO GdkWMDecoration
gdkWindowGetDecorations (GdkWindow p) = GdkWMDecoration <$> c_gdk_window_get_decorations p

foreign import ccall "gdk_get_default_root_window" c_gdk_get_default_root_window ::
	IO (Ptr GdkWindow)

gdkGetDefaultRootWindow :: IO GdkWindow
gdkGetDefaultRootWindow = GdkWindow <$> c_gdk_get_default_root_window

foreign import ccall "gdk_window_set_device_cursor" c_gdk_window_set_device_cursor ::
	Ptr GdkWindow -> Ptr GdkDevice -> Ptr GdkCursor -> IO ()

gdkWindowSetDeviceCursor :: GdkWindow -> GdkDevice -> GdkCursor -> IO ()
gdkWindowSetDeviceCursor (GdkWindow w) (GdkDevice d) (GdkCursor fc) =
	withForeignPtr fc \c -> c_gdk_window_set_device_cursor w d c

foreign import ccall "gdk_window_set_device_events" c_gdk_window_set_device_events ::
	Ptr GdkWindow -> Ptr GdkDevice -> #{type GdkEventMask} -> IO ()

gdkWindowSetDeviceEvents :: GdkWindow -> GdkDevice -> [GdkEventMask] -> IO ()
gdkWindowSetDeviceEvents (GdkWindow w) (GdkDevice d) ems =
	c_gdk_window_set_device_events w d $ mergeGdkEventMask ems

foreign import ccall "gdk_window_set_source_events" c_gdk_window_set_source_events ::
	Ptr GdkWindow -> #{type GdkInputSource} -> #{type GdkEventMask} -> IO ()

gdkWindowSetSourceEvents :: GdkWindow -> GdkInputSource -> [GdkEventMask] -> IO ()
gdkWindowSetSourceEvents (GdkWindow w) (GdkInputSource is) ems =
	c_gdk_window_set_source_events w is $ mergeGdkEventMask ems

foreign import ccall "gdk_window_set_event_compression" c_gdk_window_set_event_compression ::
	Ptr GdkWindow -> #{type gboolean} -> IO ()

gdkWindowSetEventCompression :: GdkWindow -> Bool -> IO ()
gdkWindowSetEventCompression (GdkWindow w) = c_gdk_window_set_event_compression w . boolToGboolean
