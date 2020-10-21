{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Exception
import Data.Bits
import Data.Word
import Data.Int

import Graphics.Gdk.Types
import Graphics.Gdk.Values
import Graphics.Cairo.Types

#include <gdk/gdk.h>

mergeGdkWindowAttributesType :: [GdkWindowAttributesType] -> #{type GdkWindowAttributesType}
mergeGdkWindowAttributesType [] = 0
mergeGdkWindowAttributesType (GdkWindowAttributesType at : ats) =
	at .|. mergeGdkWindowAttributesType ats

foreign import ccall "gdk_window_new" c_gdk_window_new ::
	Ptr GdkWindow -> Ptr GdkWindowAttr -> #{type GdkWindowAttributesType} -> IO (Ptr GdkWindow)

gdkWindowNew :: Maybe GdkWindow -> GdkWindowAttr -> [GdkWindowAttributesType] -> IO GdkWindow
gdkWindowNew mp (GdkWindowAttr attr) am =
	maybe ($ nullPtr) (\(GdkWindow fp) -> withForeignPtr fp) mp \p ->
	makeGdkWindow =<< c_gdk_window_new p attr (mergeGdkWindowAttributesType am)

gdkWindowNew' :: Maybe GdkWindow -> GdkWindowAttr' -> IO GdkWindow
gdkWindowNew' mp wattr = maybe ($ nullPtr) (\(GdkWindow fp) -> withForeignPtr fp) mp \p -> do
	(fwa, wam) <- newGdkWindowAttr wattr
	withForeignPtr fwa \wa -> makeGdkWindow =<< c_gdk_window_new p wa wam

foreign import ccall "gdk_window_show" c_gdk_window_show :: Ptr GdkWindow -> IO ()

gdkWindowShow :: GdkWindow -> IO ()
gdkWindowShow (GdkWindow w) = withForeignPtr w c_gdk_window_show

foreign import ccall "gdk_window_begin_draw_frame" c_gdk_window_begin_draw_frame ::
	Ptr GdkWindow -> Ptr (CairoRegionT s) -> IO (Ptr GdkDrawingContext)

foreign import ccall "gdk_window_end_draw_frame" c_gdk_window_end_draw_frame ::
	Ptr GdkWindow -> Ptr GdkDrawingContext -> IO ()

gdkWindowWithDrawFrame :: GdkWindow -> CairoRegionT s -> (GdkDrawingContext -> IO a) -> IO a
gdkWindowWithDrawFrame (GdkWindow fw) (CairoRegionT fr) act = withForeignPtr fw \w -> withForeignPtr fr \r -> bracket
	(c_gdk_window_begin_draw_frame w r) (c_gdk_window_end_draw_frame w) $ (. GdkDrawingContext) act

foreign import ccall "gdk_window_invalidate_rect" c_gdk_window_invalidate_rect ::
	Ptr GdkWindow -> Ptr GdkRectangle -> #{type gboolean} -> IO ()

gdkWindowInvalidateRect :: GdkWindow -> (#{type int}, #{type int}) -> (#{type int}, #{type int}) -> Bool -> IO ()
gdkWindowInvalidateRect (GdkWindow fwin) (x, y) (w, h) b = allocaBytes #{size GdkRectangle} \p -> do
	#{poke GdkRectangle, x} p x
	#{poke GdkRectangle, y} p y
	#{poke GdkRectangle, width} p w
	#{poke GdkRectangle, height} p h
	withForeignPtr fwin \win ->
		c_gdk_window_invalidate_rect win p $ boolToGboolean b

boolToGboolean :: Bool -> #type gboolean
boolToGboolean False = #const FALSE
boolToGboolean True = #const TRUE

foreign import ccall "gdk_window_freeze_updates" c_gdk_window_freeze_updates :: Ptr GdkWindow -> IO ()

gdkWindowFreezeUpdates :: GdkWindow -> IO ()
gdkWindowFreezeUpdates (GdkWindow fp) = withForeignPtr fp c_gdk_window_freeze_updates

foreign import ccall "gdk_window_thaw_updates" c_gdk_window_thaw_updates :: Ptr GdkWindow -> IO ()

gdkWindowThawUpdates :: GdkWindow -> IO ()
gdkWindowThawUpdates (GdkWindow fp) = withForeignPtr fp c_gdk_window_thaw_updates

foreign import ccall "gdk_window_set_events" c_gdk_window_set_events :: Ptr GdkWindow -> #{type GdkEventMask} -> IO ()

gdkWindowSetEvents :: GdkWindow -> [GdkEventMask] -> IO ()
gdkWindowSetEvents (GdkWindow fp) m = withForeignPtr fp \p -> c_gdk_window_set_events p (mergeGdkEventMask m)
