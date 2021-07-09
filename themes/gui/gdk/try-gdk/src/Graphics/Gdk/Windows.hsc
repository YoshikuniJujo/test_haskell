{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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
	gdkWindowRaise, gdkWindowLower,
	gdkWindowFocus,
	gdkWindowWithDrawFrame,
	gdkWindowGetVisibleRegion,
	gdkWindowSetTitle,
	gdkWindowSetCursor, gdkWindowGetCursor,
	gdkWindowGetGeometry,
	gdkWindowGetWidth, gdkWindowGetHeight,
	gdkWindowSetModalHint, gdkWindowGetModalHint,
	gdkWindowSetTypeHint, gdkWindowGetTypeHint,
	gdkWindowSetSkipTaskbarHint, gdkWindowSetSkipPagerHint,
	gdkWindowSetUrgencyHint,
	gdkWindowGetPosition, gdkWindowGetRootOrigin, gdkWindowGetFrameExtents,
	gdkWindowGetOrigin, gdkWindowGetRootCoords,
	gdkWindowGetParent, gdkWindowGetToplevel, gdkWindowPeekChildren,
	gdkWindowGetEvents, gdkWindowSetEvents,
	gdkWindowSetTransientFor,

	GdkWMDecoration, GdkWMDecorations, gdkWMDecorations, gdkWMDecorationList,
	pattern GdkDecorAll, pattern GdkDecorBorder, pattern GdkDecorResizeh,
	pattern GdkDecorTitle, pattern GdkDecorMenu, pattern GdkDecorMinimize,
	pattern GdkDecorMaximize,
	gdkWindowSetDecorations, gdkWindowGetDecorations,

	gdkGetDefaultRootWindow,

	gdkWindowGetSupportMultidevice, gdkWindowSetSupportMultidevice,
	gdkWindowGetDeviceCursor, gdkWindowSetDeviceCursor,

	-- * Not Checked
	gdkWindowSetDeviceEvents, gdkWindowSetSourceEvents,

	gdkWindowSetEventCompression,

	GdkWindowAttr(..), minimalGdkWindowAttr,


	-- * GdkWindowType
	GdkWindowType(..),
	pattern GdkWindowRoot, pattern GdkWindowToplevel,
	pattern GdkWindowChild, pattern GdkWindowTemp, pattern GdkWindowForeign,
	pattern GdkWindowOffscreen, pattern GdkWindowSubsurface,

	-- * GdkWindowTypeHint
	GdkWindowTypeHint,
	pattern GdkWindowTypeHintNormal, pattern GdkWindowTypeHintDialog,
	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Foreign.C.Enum
import Control.Monad.ST
import Control.Exception
import Data.Maybe
import Data.Bits
import Data.Bits.Misc
import Data.Word
import Data.Int
import System.GLib.Bool
import System.GLib.DoublyLinkedLists

import Graphics.Gdk.GdkDisplay
import {-# SOURCE #-} Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.PointsAndRectangles
import Graphics.Gdk.Visuals
import Graphics.Gdk.GdkDrawingContext
import Graphics.Gdk.Cursors
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.Types

import Graphics.Cairo.Drawing.Regions

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
	d <- gdkWindowGetDisplay w
	dvps <- allPointerDevice d
	cs <- catMaybes <$> mapM (c_gdk_window_get_device_cursor' w) dvps
	mapM_ c_g_object_unref cs
	c_gdk_window_destroy w

allPointerDevice :: GdkDisplay -> IO [GdkDevice]
allPointerDevice d = mapM gdkSeatGetPointer =<< gdkDisplayListSeats d

foreign import ccall "gdk_window_destroy"
	c_gdk_window_destroy :: GdkWindow -> IO ()

c_gdk_window_get_cursor' :: GdkWindow -> IO (Maybe (Ptr GdkCursor))
c_gdk_window_get_cursor' w = (<$> c_gdk_window_get_cursor w) \case
	NullPtr -> Nothing; p -> Just p

foreign import ccall "gdk_window_get_device_cursor"
	c_gdk_window_get_device_cursor ::
		GdkWindow -> GdkDevice -> IO (Ptr GdkCursor)

c_gdk_window_get_device_cursor' ::
	GdkWindow -> GdkDevice -> IO (Maybe (Ptr GdkCursor))
c_gdk_window_get_device_cursor' w dv = (<$> c_gdk_window_get_device_cursor w dv) \case
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

foreign import ccall "gdk_window_raise" gdkWindowRaise :: GdkWindow -> IO ()
foreign import ccall "gdk_window_lower" gdkWindowLower :: GdkWindow -> IO ()

foreign import ccall "gdk_window_focus"
	gdkWindowFocus :: GdkWindow -> Word32 -> IO ()

gdkWindowWithDrawFrame :: GdkWindow ->
	CairoRegionT s -> (forall t . GdkDrawingContext t -> IO a) -> IO a
gdkWindowWithDrawFrame w (CairoRegionT fr) act = withForeignPtr fr \r -> bracket
	(c_gdk_window_begin_draw_frame w r) (c_gdk_window_end_draw_frame w) act

foreign import ccall "gdk_window_begin_draw_frame"
	c_gdk_window_begin_draw_frame ::
		GdkWindow -> Ptr (CairoRegionT s) -> IO (GdkDrawingContext s)

foreign import ccall "gdk_window_end_draw_frame"
	c_gdk_window_end_draw_frame ::
		GdkWindow -> GdkDrawingContext s -> IO ()

gdkWindowGetVisibleRegion :: GdkWindow -> IO (CairoRegionT RealWorld)
gdkWindowGetVisibleRegion w = CairoRegionT <$> do
	pr <- c_gdk_window_get_visible_region w
	newForeignPtr pr $ c_cairo_region_destroy pr

foreign import ccall "gdk_window_get_visible_region"
	c_gdk_window_get_visible_region ::
		GdkWindow -> IO (Ptr (CairoRegionT s))

gdkWindowSetTitle :: GdkWindow -> String -> IO ()
gdkWindowSetTitle w t = withCString t \ct -> c_gdk_window_set_title w ct

foreign import ccall "gdk_window_set_title"
	c_gdk_window_set_title :: GdkWindow -> CString -> IO ()

gdkWindowSetCursor :: GdkWindow -> GdkCursor -> IO ()
gdkWindowSetCursor w (GdkCursor fc) = do
	whenMaybe c_g_object_unref =<< c_gdk_window_get_cursor' w
	withForeignPtr fc \c -> c_g_object_ref c >> c_gdk_window_set_cursor w c

foreign import ccall "gdk_window_set_cursor"
	c_gdk_window_set_cursor :: GdkWindow -> Ptr GdkCursor -> IO ()

gdkWindowGetCursor :: GdkWindow -> IO (Maybe GdkCursor)
gdkWindowGetCursor w = c_gdk_window_get_cursor w >>= \case
	NullPtr -> pure Nothing
	c -> Just . GdkCursor
		<$> (c_g_object_ref c >> newForeignPtr c (c_g_object_unref c))

gdkWindowGetGeometry :: GdkWindow -> IO (CInt, CInt, CInt, CInt)
gdkWindowGetGeometry wn = alloca \x -> alloca \y -> alloca \w -> alloca \h -> do
	c_gdk_window_get_geometry wn x y w h
	(,,,) <$> peek x <*> peek y <*> peek w <*> peek h

foreign import ccall "gdk_window_get_geometry"
	c_gdk_window_get_geometry ::
		GdkWindow ->
		Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall "gdk_window_get_width"
	gdkWindowGetWidth :: GdkWindow -> IO CInt

foreign import ccall "gdk_window_get_height"
	gdkWindowGetHeight :: GdkWindow -> IO CInt

gdkWindowSetModalHint :: GdkWindow -> Bool -> IO ()
gdkWindowSetModalHint w = c_gdk_window_set_modal_hint w . boolToGboolean

foreign import ccall "gdk_window_set_modal_hint"
	c_gdk_window_set_modal_hint :: GdkWindow -> #{type gboolean} -> IO ()

gdkWindowGetModalHint :: GdkWindow -> IO Bool
gdkWindowGetModalHint w = gbooleanToBool <$> c_gdk_window_get_modal_hint w

foreign import ccall "gdk_window_get_modal_hint"
	c_gdk_window_get_modal_hint :: GdkWindow -> IO #{type gboolean}

enum "GdkWindowTypeHint" ''#{type GdkWindowTypeHint} [''Show, ''Read] [
	("GdkWindowTypeHintNormal", #{const GDK_WINDOW_TYPE_HINT_NORMAL}),
	("GdkWindowTypeHintDialog", #{const GDK_WINDOW_TYPE_HINT_DIALOG}),
	("GdkWindowTypeHintMenu", #{const GDK_WINDOW_TYPE_HINT_MENU}),
	("GdkWindowTypeHintToolbar", #{const GDK_WINDOW_TYPE_HINT_TOOLBAR}),
	("GdkWindowTypeHintSplashscreen",
		#{const GDK_WINDOW_TYPE_HINT_SPLASHSCREEN}),
	("GdkWindowTypeHintUtility", #{const GDK_WINDOW_TYPE_HINT_UTILITY}),
	("GdkWindowTypeHintDock", #{const GDK_WINDOW_TYPE_HINT_DOCK}),
	("GdkWindowTypeHintDesktop", #{const GDK_WINDOW_TYPE_HINT_DESKTOP}),
	("GdkWindowTypeHintDropdownMenu",
		#{const GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU}),
	("GdkWindowTypeHintPopupMenu",
		#{const GDK_WINDOW_TYPE_HINT_POPUP_MENU}),
	("GdkWindowTypeHintTooltip", #{const GDK_WINDOW_TYPE_HINT_TOOLTIP}),
	("GdkWindowTypeHintNotification",
		#{const GDK_WINDOW_TYPE_HINT_NOTIFICATION}),
	("GdkWindowTypeHintCombo", #{const GDK_WINDOW_TYPE_HINT_COMBO}),
	("GdkWindowTypeHintDnd", #{const GDK_WINDOW_TYPE_HINT_DND}) ]

foreign import ccall "gdk_window_set_type_hint"
	gdkWindowSetTypeHint :: GdkWindow -> GdkWindowTypeHint -> IO ()

foreign import ccall "gdk_window_get_type_hint"
	gdkWindowGetTypeHint :: GdkWindow -> IO GdkWindowTypeHint

gdkWindowSetSkipTaskbarHint :: GdkWindow -> Bool -> IO ()
gdkWindowSetSkipTaskbarHint w =
	c_gdk_window_set_skip_taskbar_hint w . boolToGboolean

foreign import ccall "gdk_window_set_skip_taskbar_hint"
	c_gdk_window_set_skip_taskbar_hint ::
		GdkWindow -> #{type gboolean} -> IO ()

gdkWindowSetSkipPagerHint :: GdkWindow -> Bool -> IO ()
gdkWindowSetSkipPagerHint w =
	c_gdk_window_set_skip_pager_hint w . boolToGboolean

foreign import ccall "gdk_window_set_skip_pager_hint"
	c_gdk_window_set_skip_pager_hint ::
		GdkWindow -> #{type gboolean} -> IO ()

gdkWindowSetUrgencyHint :: GdkWindow -> Bool -> IO ()
gdkWindowSetUrgencyHint w = c_gdk_window_set_urgency_hint w . boolToGboolean

foreign import ccall "gdk_window_set_urgency_hint"
	c_gdk_window_set_urgency_hint ::
		GdkWindow -> #{type gboolean} -> IO ()

gdkWindowGetPosition :: GdkWindow -> IO (CInt, CInt)
gdkWindowGetPosition w = alloca \x -> alloca \y -> do
	c_gdk_window_get_position w x y
	(,) <$> peek x <*> peek y

foreign import ccall "gdk_window_get_position" c_gdk_window_get_position ::
	GdkWindow -> Ptr CInt -> Ptr CInt -> IO ()

gdkWindowGetRootOrigin :: GdkWindow -> IO (CInt, CInt)
gdkWindowGetRootOrigin w = alloca \x -> alloca \y -> do
	c_gdk_window_get_root_origin w x y
	(,) <$> peek x <*> peek y

foreign import ccall "gdk_window_get_root_origin"
	c_gdk_window_get_root_origin ::
		GdkWindow -> Ptr CInt -> Ptr CInt -> IO ()

gdkWindowGetFrameExtents :: GdkWindow -> GdkRectangleIO -> IO ()
gdkWindowGetFrameExtents w (GdkRectanglePrim fr) =
	withForeignPtr fr $ c_gdk_window_get_frame_extents w

foreign import ccall "gdk_window_get_frame_extents"
	c_gdk_window_get_frame_extents :: GdkWindow -> Ptr GdkRectangle -> IO ()

gdkWindowGetOrigin :: GdkWindow -> IO (CInt, CInt)
gdkWindowGetOrigin w = alloca \x -> alloca \y -> do
	c_gdk_window_get_origin w x y
	(,) <$> peek x <*> peek y

foreign import ccall "gdk_window_get_origin"
	c_gdk_window_get_origin :: GdkWindow -> Ptr CInt -> Ptr CInt -> IO ()

gdkWindowGetRootCoords :: GdkWindow -> CInt -> CInt -> IO (CInt, CInt)
gdkWindowGetRootCoords w x y = alloca \x' -> alloca \y' -> do
	c_gdk_window_get_root_coords w x y x' y'
	(,) <$> peek x' <*> peek y'

foreign import ccall "gdk_window_get_root_coords"
	c_gdk_window_get_root_coords ::
		GdkWindow -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall "gdk_window_get_parent"
	gdkWindowGetParent :: GdkWindow -> IO GdkWindow

foreign import ccall "gdk_window_get_toplevel"
	gdkWindowGetToplevel :: GdkWindow -> IO GdkWindow

gdkWindowPeekChildren :: GdkWindow -> IO [GdkWindow]
gdkWindowPeekChildren w =
	map GdkWindow <$> (g_list_to_list =<< c_gdk_window_peek_children w)

foreign import ccall "gdk_window_peek_children"
	c_gdk_window_peek_children :: GdkWindow -> IO (Ptr (GList GdkWindow))

foreign import ccall "gdk_window_get_events"
	gdkWindowGetEvents :: GdkWindow -> IO GdkEventMaskMultiBits

foreign import ccall "gdk_window_set_events"
	gdkWindowSetEvents :: GdkWindow -> GdkEventMaskMultiBits -> IO ()

foreign import ccall "gdk_window_set_transient_for"
	gdkWindowSetTransientFor :: GdkWindow -> GdkWindow -> IO ()

enum "GdkWMDecoration" ''#{type GdkWMDecoration} [''Show, ''Read] [
	("GdkDecorAll", #{const GDK_DECOR_ALL}),
	("GdkDecorBorder", #{const GDK_DECOR_BORDER}),
	("GdkDecorResizeh", #{const GDK_DECOR_RESIZEH}),
	("GdkDecorTitle", #{const GDK_DECOR_TITLE}),
	("GdkDecorMenu", #{const GDK_DECOR_MENU}),
	("GdkDecorMinimize", #{const GDK_DECOR_MINIMIZE}),
	("GdkDecorMaximize", #{const GDK_DECOR_MAXIMIZE}) ]

newtype GdkWMDecorations = GdkWMDecorations #{type GdkWMDecoration} deriving Show

getGdkWMDecoration :: GdkWMDecoration -> #{type GdkWMDecoration}
getGdkWMDecoration (GdkWMDecoration wmd) = wmd

getGdkWMDecorations :: GdkWMDecorations -> #{type GdkWMDecoration}
getGdkWMDecorations (GdkWMDecorations wmd) = wmd

gdkWMDecorations :: [GdkWMDecoration] -> GdkWMDecorations
gdkWMDecorations = GdkWMDecorations . foldr (.|.) 0 . map getGdkWMDecoration

gdkWMDecorationList :: GdkWMDecorations -> [GdkWMDecoration]
gdkWMDecorationList = map GdkWMDecoration
	. separateBits (#{size GdkWMDecoration} * 8) . getGdkWMDecorations

foreign import ccall "gdk_window_set_decorations"
	gdkWindowSetDecorations :: GdkWindow -> GdkWMDecorations -> IO ()

gdkWindowGetDecorations :: GdkWindow -> IO (Maybe GdkWMDecorations)
gdkWindowGetDecorations w = alloca \d -> do
	c_gdk_window_get_decorations w d >>= \case
		#{const FALSE} -> pure Nothing
		#{const TRUE} -> Just . GdkWMDecorations <$> peek d
		_ -> error "gboolean should be FALSE or TRUE"

foreign import ccall "gdk_window_get_decorations"
	c_gdk_window_get_decorations :: GdkWindow -> Ptr #{type GdkWMDecoration} -> IO #{type gboolean}

foreign import ccall "gdk_get_default_root_window"
	gdkGetDefaultRootWindow :: IO GdkWindow

gdkWindowGetSupportMultidevice :: GdkWindow -> IO Bool
gdkWindowGetSupportMultidevice w =
	gbooleanToBool <$> c_gdk_window_get_support_multidevice w

foreign import ccall "gdk_window_get_support_multidevice"
	c_gdk_window_get_support_multidevice :: GdkWindow -> IO #{type gboolean}

gdkWindowSetSupportMultidevice :: GdkWindow -> Bool -> IO ()
gdkWindowSetSupportMultidevice w =
	c_gdk_window_set_support_multidevice w . boolToGboolean

foreign import ccall "gdk_window_set_support_multidevice"
	c_gdk_window_set_support_multidevice ::
		GdkWindow -> #{type gboolean} -> IO ()

gdkWindowGetDeviceCursor :: GdkWindow -> GdkDevice -> IO (Maybe GdkCursor)
gdkWindowGetDeviceCursor w dv = c_gdk_window_get_device_cursor w dv >>= \case
	NullPtr -> pure Nothing
	c -> Just . GdkCursor
		<$> (c_g_object_ref c >> newForeignPtr c (c_g_object_unref c))

gdkWindowSetDeviceCursor :: GdkWindow -> GdkDevice -> GdkCursor -> IO ()
gdkWindowSetDeviceCursor w d (GdkCursor fc) = do
	whenMaybe c_g_object_unref =<< c_gdk_window_get_device_cursor' w d
	withForeignPtr fc $ \c -> c_g_object_ref c >> c_gdk_window_set_device_cursor w d c

foreign import ccall "gdk_window_set_device_cursor"
	c_gdk_window_set_device_cursor ::
		GdkWindow -> GdkDevice -> Ptr GdkCursor -> IO ()

foreign import ccall "gdk_window_set_device_events" c_gdk_window_set_device_events ::
	Ptr GdkWindow -> Ptr GdkDevice -> #{type GdkEventMask} -> IO ()

gdkWindowSetDeviceEvents :: GdkWindow -> GdkDevice -> [GdkEventMaskSingleBit] -> IO ()
gdkWindowSetDeviceEvents (GdkWindow w) (GdkDevice d) ems =
	c_gdk_window_set_device_events w d $ mergeGdkEventMask ems

foreign import ccall "gdk_window_set_source_events" c_gdk_window_set_source_events ::
	Ptr GdkWindow -> #{type GdkInputSource} -> #{type GdkEventMask} -> IO ()

gdkWindowSetSourceEvents :: GdkWindow -> GdkInputSource -> [GdkEventMaskSingleBit] -> IO ()
gdkWindowSetSourceEvents (GdkWindow w) (GdkInputSource is) ems =
	c_gdk_window_set_source_events w is $ mergeGdkEventMask ems

foreign import ccall "gdk_window_set_event_compression" c_gdk_window_set_event_compression ::
	Ptr GdkWindow -> #{type gboolean} -> IO ()

gdkWindowSetEventCompression :: GdkWindow -> Bool -> IO ()
gdkWindowSetEventCompression (GdkWindow w) = c_gdk_window_set_event_compression w . boolToGboolean
