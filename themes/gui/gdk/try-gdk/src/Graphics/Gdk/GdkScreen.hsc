{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkScreen where

import Foreign.Ptr
import Data.Int

import Graphics.Gdk.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_screen_get_default" c_gdk_screen_get_default ::
	IO (Ptr GdkScreen)

gdkScreenGetDefault :: IO GdkScreen
gdkScreenGetDefault = GdkScreen <$> c_gdk_screen_get_default

foreign import ccall "gdk_screen_get_system_visual" c_gdk_screen_get_system_visual ::
	Ptr GdkScreen -> IO (Ptr GdkVisual)

gdkScreenGetSystemVisual :: GdkScreen -> IO GdkVisual
gdkScreenGetSystemVisual (GdkScreen p) = GdkVisual <$> c_gdk_screen_get_system_visual p

foreign import ccall "gdk_screen_get_rgba_visual" c_gdk_screen_get_rgba_visual ::
	Ptr GdkScreen -> IO (Ptr GdkVisual)

gdkScreenGetRgbaVisual :: GdkScreen -> IO (Maybe GdkVisual)
gdkScreenGetRgbaVisual (GdkScreen s) = (<$> c_gdk_screen_get_system_visual s) \case
	v	| v == nullPtr -> Nothing
		| otherwise -> Just $ GdkVisual v

foreign import ccall "gdk_screen_is_composited" c_gdk_screen_is_composited ::
	Ptr GdkScreen -> IO #type gboolean

gdkScreenIsComposited :: GdkScreen -> IO #type gboolean
gdkScreenIsComposited (GdkScreen s) = c_gdk_screen_is_composited s

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

foreign import ccall "gdk_screen_get_resolution" c_gdk_screen_get_resolution ::
	Ptr GdkScreen -> IO #type gdouble

gdkScreenGetResolution :: GdkScreen -> IO Double
gdkScreenGetResolution (GdkScreen p) = c_gdk_screen_get_resolution p
