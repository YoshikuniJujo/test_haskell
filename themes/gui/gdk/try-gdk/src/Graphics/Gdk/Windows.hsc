{-# LANGUAGE BlockArguments #-}
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

	gdkWindowSetCursor, gdkWindowSetTitle
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Control.Exception
import Data.Word
import Data.Int

import Graphics.Gdk.Types
import Graphics.Gdk.Values
import Graphics.Cairo.Types

#include <gdk/gdk.h>

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

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

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

boolToGboolean :: Bool -> #type gboolean
boolToGboolean False = #const FALSE
boolToGboolean True = #const TRUE

foreign import ccall "gdk_window_freeze_updates" c_gdk_window_freeze_updates :: Ptr GdkWindow -> IO ()

gdkWindowFreezeUpdates :: GdkWindow -> IO ()
gdkWindowFreezeUpdates (GdkWindow p) = c_gdk_window_freeze_updates p

foreign import ccall "gdk_window_thaw_updates" c_gdk_window_thaw_updates :: Ptr GdkWindow -> IO ()

gdkWindowThawUpdates :: GdkWindow -> IO ()
gdkWindowThawUpdates (GdkWindow p) = c_gdk_window_thaw_updates p

foreign import ccall "gdk_window_set_events" c_gdk_window_set_events :: Ptr GdkWindow -> #{type GdkEventMask} -> IO ()

gdkWindowSetEvents :: GdkWindow -> [GdkEventMask] -> IO ()
gdkWindowSetEvents (GdkWindow p) m = c_gdk_window_set_events p (mergeGdkEventMask m)

foreign import ccall "gdk_window_set_cursor" c_gdk_window_set_cursor :: Ptr GdkWindow -> Ptr GdkCursor -> IO ()

gdkWindowSetCursor :: GdkWindow -> GdkCursor -> IO ()
gdkWindowSetCursor (GdkWindow w) (GdkCursor fc) = withForeignPtr fc \c ->
	c_gdk_window_set_cursor w c

foreign import ccall "gdk_window_set_title" c_gdk_window_set_title :: Ptr GdkWindow -> CString -> IO ()

gdkWindowSetTitle :: GdkWindow -> String -> IO ()
gdkWindowSetTitle (GdkWindow p) ttl = withCString ttl \cttl ->
	c_gdk_window_set_title p cttl
