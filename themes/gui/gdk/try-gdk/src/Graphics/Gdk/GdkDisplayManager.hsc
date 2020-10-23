{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDisplayManager where

import Foreign.Ptr

import Graphics.Gdk.Types

#include <gdk/gdk.h>

foreign import ccall "gdk_display_manager_get" c_gdk_display_manager_get :: IO (Ptr GdkDisplayManager)

gdkDisplayManagerGet :: IO GdkDisplayManager
gdkDisplayManagerGet = GdkDisplayManager <$> c_gdk_display_manager_get

foreign import ccall "gdk_display_manager_get_default_display" c_gdk_display_manager_get_default_display ::
	Ptr GdkDisplayManager -> IO (Ptr GdkDisplay)

gdkDisplayManagerGetDefaultDisplay :: GdkDisplayManager -> IO GdkDisplay
gdkDisplayManagerGetDefaultDisplay (GdkDisplayManager p) =
	GdkDisplay <$> c_gdk_display_manager_get_default_display p

foreign import ccall "gdk_display_manager_set_default_display" c_gdk_display_manager_set_default_display ::
	Ptr GdkDisplayManager -> Ptr GdkDisplay -> IO ()

gdkDisplayManagerSetDefaultDisplay :: GdkDisplayManager -> GdkDisplay -> IO ()
gdkDisplayManagerSetDefaultDisplay (GdkDisplayManager dm) (GdkDisplay d) =
	c_gdk_display_manager_set_default_display dm d
