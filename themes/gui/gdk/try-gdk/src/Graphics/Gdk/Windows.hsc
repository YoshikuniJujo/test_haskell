{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception
import Data.Bits
import Data.Word

import Graphics.Gdk.Types
import Graphics.Gdk.Values

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

foreign import ccall "gdk_window_show" c_gdk_window_show :: Ptr GdkWindow -> IO ()

gdkWindowShow :: GdkWindow -> IO ()
gdkWindowShow (GdkWindow w) = withForeignPtr w c_gdk_window_show

foreign import ccall "gdk_window_begin_draw_frame" c_gdk_window_begin_draw_frame ::
	Ptr GdkWindow -> Ptr CairoRegionT -> IO (Ptr GdkDrawingContext)

foreign import ccall "gdk_window_end_draw_frame" c_gdk_window_end_draw_frame ::
	Ptr GdkWindow -> Ptr GdkDrawingContext -> IO ()

gdkWindowWithDrawFrame :: GdkWindow -> CairoRegionT -> (GdkDrawingContext -> IO a) -> IO a
gdkWindowWithDrawFrame (GdkWindow fw) (CairoRegionT r) act = withForeignPtr fw \w -> bracket
	(c_gdk_window_begin_draw_frame w r) (c_gdk_window_end_draw_frame w) $ (. GdkDrawingContext) act
