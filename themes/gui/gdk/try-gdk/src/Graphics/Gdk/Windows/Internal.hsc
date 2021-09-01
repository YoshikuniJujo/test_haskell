{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows.Internal (
	-- * GDK WINDOW AND GDK WINDOW AUTO UNREF
	GdkWindow(..), GdkWindowAutoUnref(..), withGdkWindowAutoUnref,
	GdkWindowNeedUnref, mkGdkWindowAutoUnref,

	-- * NEW, DESTROY AND SHOW
	gdkWindowNew, gdkToplevelNew, gdkWindowDestroy,
	gdkWindowShow, gdkWindowShowUnraised, gdkWindowHide,

	-- * DISPLAY, SCREEN, VISUAL AND WINDOW
	gdkWindowGetDisplay, gdkWindowGetScreen, gdkWindowGetVisual,
	gdkGetDefaultRootWindow, gdkWindowGetToplevel,
	gdkWindowGetParent, gdkWindowReparent, gdkWindowPeekChildren,

	-- * EVENT
	-- ** Event Mask
	gdkWindowGetEvents, gdkWindowSetEvents,
	-- ** Event Compression
	gdkWindowGetEventCompression, gdkWindowSetEventCompression,

	-- * TITLE AND CURSOR
	gdkWindowSetTitle, gdkWindowSetCursor, gdkWindowGetCursor,

	-- * WITH DRAW FRAME
	gdkWindowWithDrawFrame, gdkWindowGetVisibleRegion,

	-- * WINDOW TYPE
	gdkWindowGetWindowType,

	-- * IS DESTROYED, VISIBLE, VIEWABLE, INPUT ONLY OR SHAPED
	gdkWindowIsDestroyed, gdkWindowIsVisible, gdkWindowIsViewable,
	gdkWindowIsInputOnly, gdkWindowIsShaped,

	-- * GDK WINDOW STATES
	gdkWindowGetState,
	gdkWindowWithdraw,
	gdkWindowIconify, gdkWindowDeiconify, gdkWindowStick, gdkWindowUnstick,
	gdkWindowMaximize, gdkWindowUnmaximize,
	gdkWindowFullscreen, gdkWindowUnfullscreen,
	-- ** GDK FULLSCREEN MODE
	GdkFullscreenMode(..),
	pattern GdkFullscreenOnCurrentMonitor,
	pattern GdkFullscreenOnAllMonitors,
	gdkWindowGetFullscreenMode, gdkWindowSetFullscreenMode,

	-- * GEOMETRY AND OPACITY
	gdkWindowSetKeepAbove, gdkWindowSetKeepBelow,

	gdkWindowMove, gdkWindowResize, gdkWindowMoveResize,
	gdkWindowRaise, gdkWindowLower, gdkWindowFocus,

	gdkWindowGetGeometry,
	gdkWindowGetWidth, gdkWindowGetHeight,
	gdkWindowGetPosition, gdkWindowGetRootOrigin, gdkWindowGetFrameExtents,
	gdkWindowGetOrigin, gdkWindowGetRootCoords,

	gdkWindowSetOpacity,

	-- * WINDOW BEHAVIER AND APPEARANCE
	-- ** Modal Hint
	gdkWindowSetModalHint, gdkWindowGetModalHint,
	-- ** Window Type Hint
	gdkWindowSetTypeHint, gdkWindowGetTypeHint,
	-- ** Task Bar, Pager and Urgency
	gdkWindowSetSkipTaskbarHint, gdkWindowSetSkipPagerHint,
	gdkWindowSetUrgencyHint,
	-- ** Transient For
	gdkWindowSetTransientFor,
	-- ** Gdk Wm Decoration
	gdkWindowSetDecorations, gdkWindowGetDecorations,
	GdkWmDecoration, GdkWmDecorations, gdkWmDecorations, gdkWmDecorationList,
	pattern GdkDecorAll, pattern GdkDecorBorder, pattern GdkDecorResizeh,
	pattern GdkDecorTitle, pattern GdkDecorMenu, pattern GdkDecorMinimize,
	pattern GdkDecorMaximize,

	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal hiding (void)
import Foreign.Storable
import Foreign.C
import Foreign.C.Enum
import Control.Monad
import Control.Monad.ST
import Control.Exception
import Data.Bits
import Data.Bits.Misc
import Data.Word
import Data.Int
import System.IO.Unsafe
import System.GLib.Bool
import System.GLib.DoublyLinkedLists

import Graphics.Gdk.GdkDisplay.Internal
import {-# SOURCE #-} Graphics.Gdk.GdkScreen.Internal
import Graphics.Gdk.PointsAndRectangles.Internal
import Graphics.Gdk.Visuals.Internal
import Graphics.Gdk.GdkDrawingContext.Internal
import Graphics.Gdk.Cursors.Internal
import Graphics.Gdk.Windows.GdkWindowAttr.Internal
import Graphics.Gdk.Windows.GdkEventMask.Internal
import Graphics.Gdk.EventStructures.Internal

import Graphics.Cairo.Drawing.Regions

#include <gdk/gdk.h>

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newtype GdkWindowAutoUnref = GdkWindowAutoUnref (ForeignPtr GdkWindowNeedUnref) deriving Show
data GdkWindowNeedUnref

mkGdkWindowAutoUnref :: Ptr GdkWindowNeedUnref -> IO GdkWindowAutoUnref
mkGdkWindowAutoUnref p = GdkWindowAutoUnref <$> newForeignPtr p (c_g_object_unref p)

newtype GdkWindow = GdkWindow (Ptr GdkWindow) deriving (Show, Storable)

withGdkWindowAutoUnref :: GdkWindowAutoUnref -> (GdkWindow -> IO a) -> IO ()
withGdkWindowAutoUnref (GdkWindowAutoUnref fwnu) f = void
	$ withForeignPtr fwnu \pwnu -> f (GdkWindow $ castPtr pwnu)

gdkWindowNew :: Maybe GdkWindow -> GdkWindowAttr -> IO (Maybe GdkWindow)
gdkWindowNew mw a = maybeGdkWindow <$> maybe ($ nullPtr) (\(GdkWindow pw) -> ($ pw)) mw \pw -> do
	whenMaybe cursorRef $ gdkWindowAttrCursor a
	withGdkWindowAttr a \pa ts -> c_gdk_window_new pw pa ts
	where cursorRef (GdkCursor fc) = withForeignPtr fc c_g_object_ref

maybeGdkWindow :: Ptr GdkWindow -> Maybe GdkWindow
maybeGdkWindow = \case NullPtr -> Nothing; p -> Just $ GdkWindow p

gdkToplevelNew :: Maybe GdkWindow -> GdkWindowAttr -> IO GdkWindow
gdkToplevelNew mw a = GdkWindow <$> maybe ($ nullPtr) (\(GdkWindow pw) -> ($ pw)) mw \pw -> do
	whenMaybe cursorRef $ gdkWindowAttrCursor $ a { gdkWindowAttrWindowType = GdkWindowToplevel }
	withGdkWindowAttr a \pa ts -> c_gdk_window_new pw pa ts
	where cursorRef (GdkCursor fc) = withForeignPtr fc c_g_object_ref

foreign import ccall "gdk_window_new" c_gdk_window_new ::
	Ptr GdkWindow -> Ptr GdkWindowAttr -> GdkWindowAttributesTypes ->
	IO (Ptr GdkWindow)

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

gdkWindowGetDisplay :: GdkWindow -> GdkDisplay
gdkWindowGetDisplay = unsafePerformIO . c_gdk_window_get_display

foreign import ccall "gdk_window_get_display"
	c_gdk_window_get_display :: GdkWindow -> IO GdkDisplay

gdkWindowGetScreen :: GdkWindow -> GdkScreen
gdkWindowGetScreen = unsafePerformIO . c_gdk_window_get_screen

foreign import ccall "gdk_window_get_screen"
	c_gdk_window_get_screen :: GdkWindow -> IO GdkScreen

gdkWindowGetVisual :: GdkWindow -> GdkVisual
gdkWindowGetVisual = unsafePerformIO . c_gdk_window_get_visual

foreign import ccall "gdk_window_get_visual"
	c_gdk_window_get_visual :: GdkWindow -> IO GdkVisual

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

enum "GdkFullscreenMode" ''#{type GdkFullscreenMode} [''Show, ''Read, ''Eq] [
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
	gdkWindowFocus :: GdkWindow -> MilliSecond -> IO ()

gdkWindowWithDrawFrame :: GdkWindow ->
	CairoRegionT RealWorld -> (forall t . GdkDrawingContext t -> IO a) -> IO a
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

gdkWindowPeekChildren :: GdkWindow -> IO (Maybe [GdkWindow])
gdkWindowPeekChildren w =
	(map GdkWindow <$>) <$> (g_list_to_list =<< c_gdk_window_peek_children w)

foreign import ccall "gdk_window_peek_children"
	c_gdk_window_peek_children :: GdkWindow -> IO (Ptr (GList GdkWindow))

foreign import ccall "gdk_window_get_events"
	gdkWindowGetEvents :: GdkWindow -> IO GdkEventMaskMultiBits

foreign import ccall "gdk_window_set_events"
	gdkWindowSetEvents :: GdkWindow -> GdkEventMaskMultiBits -> IO ()

foreign import ccall "gdk_window_set_transient_for"
	gdkWindowSetTransientFor :: GdkWindow -> GdkWindow -> IO ()

enum "GdkWmDecoration" ''#{type GdkWMDecoration} [''Show, ''Read, ''Eq] [
	("GdkDecorAll", #{const GDK_DECOR_ALL}),
	("GdkDecorBorder", #{const GDK_DECOR_BORDER}),
	("GdkDecorResizeh", #{const GDK_DECOR_RESIZEH}),
	("GdkDecorTitle", #{const GDK_DECOR_TITLE}),
	("GdkDecorMenu", #{const GDK_DECOR_MENU}),
	("GdkDecorMinimize", #{const GDK_DECOR_MINIMIZE}),
	("GdkDecorMaximize", #{const GDK_DECOR_MAXIMIZE}) ]

newtype GdkWmDecorations = GdkWmDecorations #{type GdkWMDecoration} deriving Show

getGdkWmDecoration :: GdkWmDecoration -> #{type GdkWMDecoration}
getGdkWmDecoration (GdkWmDecoration wmd) = wmd

getGdkWmDecorations :: GdkWmDecorations -> #{type GdkWMDecoration}
getGdkWmDecorations (GdkWmDecorations wmd) = wmd

gdkWmDecorations :: [GdkWmDecoration] -> GdkWmDecorations
gdkWmDecorations = GdkWmDecorations . foldr (.|.) 0 . map getGdkWmDecoration

gdkWmDecorationList :: GdkWmDecorations -> [GdkWmDecoration]
gdkWmDecorationList = map GdkWmDecoration
	. separateBits (#{size GdkWMDecoration} * 8) . getGdkWmDecorations

foreign import ccall "gdk_window_set_decorations"
	gdkWindowSetDecorations :: GdkWindow -> GdkWmDecorations -> IO ()

gdkWindowGetDecorations :: GdkWindow -> IO (Maybe GdkWmDecorations)
gdkWindowGetDecorations w = alloca \d -> do
	c_gdk_window_get_decorations w d >>= \case
		#{const FALSE} -> pure Nothing
		#{const TRUE} -> Just . GdkWmDecorations <$> peek d
		_ -> error "gboolean should be FALSE or TRUE"

foreign import ccall "gdk_window_get_decorations"
	c_gdk_window_get_decorations :: GdkWindow -> Ptr #{type GdkWMDecoration} -> IO #{type gboolean}

foreign import ccall "gdk_get_default_root_window"
	gdkGetDefaultRootWindow :: IO GdkWindow

gdkWindowGetEventCompression :: GdkWindow -> IO Bool
gdkWindowGetEventCompression w =
	gbooleanToBool <$> c_gdk_window_get_event_compression w

foreign import ccall "gdk_window_get_event_compression"
	c_gdk_window_get_event_compression ::
		GdkWindow -> IO #{type gboolean}

gdkWindowSetEventCompression :: GdkWindow -> Bool -> IO ()
gdkWindowSetEventCompression w =
	c_gdk_window_set_event_compression w . boolToGboolean

foreign import ccall "gdk_window_set_event_compression"
	c_gdk_window_set_event_compression ::
		GdkWindow -> #{type gboolean} -> IO ()
