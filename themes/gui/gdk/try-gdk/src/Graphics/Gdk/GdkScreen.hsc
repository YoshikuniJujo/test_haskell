{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkScreen where

import Foreign.Ptr

import Graphics.Gdk.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_screen_get_default" c_gdk_screen_get_default ::
	IO (Ptr GdkScreen)

gdkScreenGetDefault :: IO GdkScreen
gdkScreenGetDefault = GdkScreen <$> c_gdk_screen_get_default

foreign import ccall "gdk_screen_get_resolution" c_gdk_screen_get_resolution ::
	Ptr GdkScreen -> IO #type gdouble

gdkScreenGetResolution :: GdkScreen -> IO #type gdouble
gdkScreenGetResolution (GdkScreen p) = c_gdk_screen_get_resolution p
