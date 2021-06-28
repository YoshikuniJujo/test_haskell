{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows (
	-- * Checked
	gdkWindowNew, gdkWindowDestroy, gdkWindowGetWindowType,
	gdkWindowGetDisplay, gdkWindowGetScreen, gdkWindowGetVisual,
	gdkWindowShow, gdkWindowShowUnraised, gdkWindowHide,
	gdkWindowIsDestroyed, gdkWindowIsVisible, gdkWindowIsViewable,
	gdkWindowIsInputOnly, gdkWindowIsShaped, gdkWindowGetState,
	gdkWindowWithdraw, gdkWindowIconify,
	gdkWindowMaximize,
	gdkWindowFullscreen,
	gdkWindowSetOpacity,

	-- * Not Checked
	gdkWindowFreezeUpdates, gdkWindowThawUpdates,
	gdkWindowWithDrawFrame, gdkWindowInvalidateRect, gdkWindowSetEvents,

	gdkWindowSetTitle, gdkWindowSetCursor, gdkWindowGetCursor,
	gdkWindowGetWidth, gdkWindowGetHeight, gdkWindowGetPosition,

	gdkWindowGetParent, gdkWindowGetDecorations, gdkGetDefaultRootWindow,
	gdkWindowSetDeviceCursor, gdkWindowSetDeviceEvents, gdkWindowSetSourceEvents,

	gdkWindowSetEventCompression,

	GdkWindowAttr(..), mkGdkWindowAttr,

	pattern GdkWindowToplevel, pattern GdkWindowRoot, pattern GdkWindowChild
	) where

import Foreign.Ptr
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

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.PointsAndRectangles
import Graphics.Gdk.Types
import Graphics.Gdk.Values
import Graphics.Gdk.EventStructures

import Graphics.Cairo.Types

import Data.Bits
import Data.Bool
import Data.Maybe

#include <gdk/gdk.h>

enum "GdkWindowType" ''#{type GdkWindowType} [''Show] [
	("GdkWindowRoot", #{const GDK_WINDOW_ROOT}),
	("GdkWindowToplevel", #{const GDK_WINDOW_TOPLEVEL}),
	("GdkWindowChild", #{const GDK_WINDOW_CHILD}),
	("GdkWindowTemp", #{const GDK_WINDOW_TEMP}),
	("GdkWindowForeign", #{const GDK_WINDOW_FOREIGN}),
	("GdkWindowOffscreen", #{const GDK_WINDOW_OFFSCREEN}),
	("GdkWindowSubsurface", #{const GDK_WINDOW_SUBSURFACE}) ]

data GdkWindowAttr = GdkWindowAttr {
	gdkWindowAttrTitle :: Maybe String,
	gdkWindowAttrEventMask :: [GdkEventMask],
	gdkWindowAttrX, gdkWindowAttrY :: Maybe #{type gint},
	gdkWindowAttrWidth, gdkWindowAttrHeight :: #{type gint},
	gdkWindowAttrWclass :: GdkWindowWindowClass,
	gdkWindowAttrVisual :: Maybe GdkVisual,
	gdkWindowAttrWindowType :: GdkWindowType,
	gdkWindowAttrCursor :: Maybe GdkCursor,
	gdkWindowAttrOverrideRedirect :: Maybe Bool,
	gdkWindowAttrTypeHint :: Maybe GdkWindowTypeHint } deriving Show

mkGdkWindowAttr ::
	[GdkEventMask] -> #{type gint} -> #{type gint} ->
	GdkWindowWindowClass -> GdkWindowType -> GdkWindowAttr
mkGdkWindowAttr em w h wc wt = GdkWindowAttr
	Nothing em Nothing Nothing w h wc Nothing wt Nothing Nothing Nothing

newtype GdkWindowTypeHint = GdkWindowTypeHint #{type GdkWindowTypeHint} deriving Show

newGdkWindowAttr :: GdkWindowAttr -> IO (ForeignPtr GdkWindowAttr, #{type GdkWindowAttributesType})
newGdkWindowAttr wattr = do
	p <- mallocBytes #{size GdkWindowAttr}
	fp <- newForeignPtr p (free p)
	fpoke fp wattr
	pure (fp, gdkWindowAttributesTypeMerged wattr)
	where
	fpoke fa a = withForeignPtr fa \pa -> do
		case gdkWindowAttrTitle a of
			Nothing -> pure ()
			Just ttl -> do
				ttl' <- newForeignCString ttl
				withForeignPtr ttl' \ttl'' ->
					#{poke GdkWindowAttr, title} pa ttl''
				addForeignPtrFinalizer fa $ touchForeignPtr ttl'
		#{poke GdkWindowAttr, event_mask} pa
			. mergeGdkEventMask $ gdkWindowAttrEventMask a
		maybe (pure ()) (#{poke GdkWindowAttr, x} pa) $ gdkWindowAttrX a
		maybe (pure ()) (#{poke GdkWindowAttr, y} pa) $ gdkWindowAttrY a
		#{poke GdkWindowAttr, width} pa $ gdkWindowAttrWidth a
		#{poke GdkWindowAttr, height} pa $ gdkWindowAttrHeight a
		#{poke GdkWindowAttr, wclass} pa
			. (\(GdkWindowWindowClass c) -> c) $ gdkWindowAttrWclass a
		maybe (pure ())
			(#{poke GdkWindowAttr, visual} pa . (\(GdkVisual v) -> v))
			(gdkWindowAttrVisual a)
		#{poke GdkWindowAttr, window_type} pa
			. (\(GdkWindowType t) -> t) $ gdkWindowAttrWindowType a
		case gdkWindowAttrCursor a of
			Nothing -> pure ()
			Just (GdkCursor fc) -> withForeignPtr fc \pc ->
				#{poke GdkWindowAttr, cursor} pa pc
		maybe (pure ())
			(#{poke GdkWindowAttr, override_redirect} pa)
			(gdkWindowAttrOverrideRedirect a)
		maybe (pure ())
			(#{poke GdkWindowAttr, type_hint} pa . (\(GdkWindowTypeHint th) -> th))
			(gdkWindowAttrTypeHint a)

gdkWindowAttributesTypeMerged :: GdkWindowAttr -> #type GdkWindowAttributesType
gdkWindowAttributesTypeMerged = merge . gdkWindowAttributesTypeList
	where merge [] = 0; merge (at : ats) = at .|. merge ats

gdkWindowAttributesTypeList :: GdkWindowAttr -> [#type GdkWindowAttributesType]
gdkWindowAttributesTypeList a = catMaybes [
	bool Nothing (Just #const GDK_WA_TITLE) . isJust $ gdkWindowAttrTitle a,
	bool Nothing (Just #const GDK_WA_X) . isJust $ gdkWindowAttrX a,
	bool Nothing (Just #const GDK_WA_Y) . isJust $ gdkWindowAttrY a,
	bool Nothing (Just #const GDK_WA_CURSOR) . isJust $ gdkWindowAttrCursor a,
	bool Nothing (Just #const GDK_WA_VISUAL) . isJust $ gdkWindowAttrVisual a,
	bool Nothing (Just #const GDK_WA_NOREDIR) . isJust $ gdkWindowAttrOverrideRedirect a,
	bool Nothing (Just #const GDK_WA_TYPE_HINT) . isJust $ gdkWindowAttrTypeHint a ]

foreign import ccall "gdk_window_new" c_gdk_window_new ::
	Ptr GdkWindow -> Ptr GdkWindowAttr -> #{type GdkWindowAttributesType} -> IO (Ptr GdkWindow)

gdkWindowNew :: Maybe GdkWindow -> GdkWindowAttr -> IO GdkWindow
gdkWindowNew mp wattr = maybe ($ nullPtr) (\(GdkWindow p) -> ($ p)) mp \p -> do
	(fwa, wam) <- newGdkWindowAttr wattr
	withForeignPtr fwa \wa -> GdkWindow <$> c_gdk_window_new p wa wam

foreign import ccall "gdk_window_destroy" c_gdk_window_destroy ::
	Ptr GdkWindow -> IO ()

gdkWindowDestroy :: GdkWindow -> IO ()
gdkWindowDestroy (GdkWindow p) = c_gdk_window_destroy p

foreign import ccall "gdk_window_get_window_type" c_gdk_window_get_window_type ::
	Ptr GdkWindow -> IO #type GdkWindowType

gdkWindowGetWindowType :: GdkWindow -> IO GdkWindowType
gdkWindowGetWindowType (GdkWindow p) = GdkWindowType <$> c_gdk_window_get_window_type p

foreign import ccall "gdk_window_get_display" c_gdk_window_get_display ::
	Ptr GdkWindow -> IO (Ptr GdkDisplay)

gdkWindowGetDisplay :: GdkWindow -> IO GdkDisplay
gdkWindowGetDisplay (GdkWindow p) = GdkDisplay <$> c_gdk_window_get_display p

foreign import ccall "gdk_window_get_screen" c_gdk_window_get_screen ::
	Ptr GdkWindow -> IO (Ptr GdkScreen)

gdkWindowGetScreen :: GdkWindow -> IO GdkScreen
gdkWindowGetScreen (GdkWindow p) = GdkScreen <$> c_gdk_window_get_screen p

foreign import ccall "gdk_window_get_visual" c_gdk_window_get_visual ::
	Ptr GdkWindow -> IO (Ptr GdkVisual)

gdkWindowGetVisual :: GdkWindow -> IO GdkVisual
gdkWindowGetVisual (GdkWindow p) = GdkVisual <$> c_gdk_window_get_visual p

foreign import ccall "gdk_window_show" c_gdk_window_show :: Ptr GdkWindow -> IO ()

gdkWindowShow :: GdkWindow -> IO ()
gdkWindowShow (GdkWindow w) = c_gdk_window_show w

foreign import ccall "gdk_window_show_unraised" c_gdk_window_show_unraised :: Ptr GdkWindow -> IO ()

gdkWindowShowUnraised :: GdkWindow -> IO ()
gdkWindowShowUnraised (GdkWindow w) = c_gdk_window_show_unraised w

foreign import ccall "gdk_window_hide" c_gdk_window_hide :: Ptr GdkWindow -> IO ()

gdkWindowHide :: GdkWindow -> IO ()
gdkWindowHide (GdkWindow w) = c_gdk_window_hide w

foreign import ccall "gdk_window_is_destroyed" c_gdk_window_is_destroyed :: Ptr GdkWindow -> IO #type gboolean

gdkWindowIsDestroyed :: GdkWindow -> IO Bool
gdkWindowIsDestroyed (GdkWindow p) = gbooleanToBool <$> c_gdk_window_is_destroyed p

foreign import ccall "gdk_window_is_visible" c_gdk_window_is_visible :: Ptr GdkWindow -> IO #type gboolean

gdkWindowIsVisible :: GdkWindow -> IO Bool
gdkWindowIsVisible (GdkWindow p) = gbooleanToBool <$> c_gdk_window_is_visible p

foreign import ccall "gdk_window_is_viewable" c_gdk_window_is_viewable :: Ptr GdkWindow -> IO #type gboolean

gdkWindowIsViewable :: GdkWindow -> IO Bool
gdkWindowIsViewable (GdkWindow p) = gbooleanToBool <$> c_gdk_window_is_viewable p

foreign import ccall "gdk_window_is_input_only" c_gdk_window_is_input_only :: Ptr GdkWindow -> IO #type gboolean

gdkWindowIsInputOnly :: GdkWindow -> IO Bool
gdkWindowIsInputOnly (GdkWindow p) = gbooleanToBool <$> c_gdk_window_is_input_only p

foreign import ccall "gdk_window_is_shaped" c_gdk_window_is_shaped :: Ptr GdkWindow -> IO #type gboolean

gdkWindowIsShaped :: GdkWindow -> IO Bool
gdkWindowIsShaped (GdkWindow p) = gbooleanToBool <$> c_gdk_window_is_shaped p

foreign import ccall "gdk_window_get_state" c_gdk_window_get_state :: Ptr GdkWindow -> IO #type GdkWindowState

gdkWindowGetState :: GdkWindow -> IO [GdkWindowState]
gdkWindowGetState (GdkWindow p) = gdkWindowStateList <$> c_gdk_window_get_state p

foreign import ccall "gdk_window_withdraw" c_gdk_window_withdraw :: Ptr GdkWindow -> IO ()

gdkWindowWithdraw :: GdkWindow -> IO ()
gdkWindowWithdraw (GdkWindow p) = c_gdk_window_withdraw p

foreign import ccall "gdk_window_iconify" c_gdk_window_iconify :: Ptr GdkWindow -> IO ()

gdkWindowIconify :: GdkWindow -> IO ()
gdkWindowIconify (GdkWindow p) = c_gdk_window_iconify p

foreign import ccall "gdk_window_maximize" c_gdk_window_maximize :: Ptr GdkWindow -> IO ()

gdkWindowMaximize :: GdkWindow -> IO ()
gdkWindowMaximize (GdkWindow p) = c_gdk_window_maximize p

foreign import ccall "gdk_window_fullscreen" c_gdk_window_fullscreen :: Ptr GdkWindow -> IO ()

gdkWindowFullscreen :: GdkWindow -> IO ()
gdkWindowFullscreen (GdkWindow p) = c_gdk_window_fullscreen p

foreign import ccall "gdk_window_set_opacity" c_gdk_window_set_opacity ::
	Ptr GdkWindow -> #{type gdouble} -> IO ()

gdkWindowSetOpacity :: GdkWindow -> #{type gdouble} -> IO ()
gdkWindowSetOpacity (GdkWindow p) o = c_gdk_window_set_opacity p o

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

foreign import ccall "gdk_window_set_title" c_gdk_window_set_title :: Ptr GdkWindow -> CString -> IO ()

gdkWindowSetTitle :: GdkWindow -> String -> IO ()
gdkWindowSetTitle (GdkWindow p) ttl = withCString ttl \cttl ->
	c_gdk_window_set_title p cttl

foreign import ccall "gdk_window_set_cursor" c_gdk_window_set_cursor :: Ptr GdkWindow -> Ptr GdkCursor -> IO ()

gdkWindowSetCursor :: GdkWindow -> GdkCursor -> IO ()
gdkWindowSetCursor (GdkWindow w) (GdkCursor fc) = withForeignPtr fc \c ->
	c_gdk_window_set_cursor w c

foreign import ccall "gdk_window_get_cursor" c_gdk_window_get_cursor :: Ptr GdkWindow -> IO (Ptr GdkCursor)

gdkWindowGetCursor :: GdkWindow -> IO GdkCursorRef
gdkWindowGetCursor (GdkWindow w) = GdkCursorRef <$> c_gdk_window_get_cursor w

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

bToGboolean :: Bool -> #{type gboolean}
bToGboolean False = #const FALSE
bToGboolean True = #const TRUE

gdkWindowSetEventCompression :: GdkWindow -> Bool -> IO ()
gdkWindowSetEventCompression (GdkWindow w) = c_gdk_window_set_event_compression w . bToGboolean
