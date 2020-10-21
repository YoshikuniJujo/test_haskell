{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows (
	-- * Checked
	gdkWindowNew, gdkWindowDestroy,

	-- * Not Checked
	gdkWindowFreezeUpdates, gdkWindowThawUpdates,
	gdkWindowWithDrawFrame, gdkWindowInvalidateRect, gdkWindowSetEvents, gdkWindowShow
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
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

foreign import ccall "gdk_window_show" c_gdk_window_show :: Ptr GdkWindow -> IO ()

gdkWindowShow :: GdkWindow -> IO ()
gdkWindowShow (GdkWindow w) = c_gdk_window_show w

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
