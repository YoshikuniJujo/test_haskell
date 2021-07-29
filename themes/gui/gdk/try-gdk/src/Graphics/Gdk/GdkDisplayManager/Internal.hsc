{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDisplayManager.Internal (
	-- * GDK DISPLAY MANAGER
	GdkDisplayManager(..),
	-- * FUNCTION
	gdkDisplayManagerGet,
	gdkDisplayManagerGetDefaultDisplay, gdkDisplayManagerSetDefaultDisplay,
	gdkDisplayManagerListDisplays, gdkDisplayManagerOpenDisplay ) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.C
import Control.Exception

import Graphics.Gdk.GdkDisplay.Internal
import Graphics.Gdk.Exception
import System.GLib.SinglyLinkedLists

#include <gdk/gdk.h>

newtype GdkDisplayManager = GdkDisplayManager (Ptr GdkDisplayManager) deriving Show

foreign import ccall "gdk_display_manager_get" c_gdk_display_manager_get :: IO (Ptr GdkDisplayManager)

gdkDisplayManagerGet :: IO GdkDisplayManager
gdkDisplayManagerGet = GdkDisplayManager <$> c_gdk_display_manager_get

foreign import ccall "gdk_display_manager_get_default_display" c_gdk_display_manager_get_default_display ::
	Ptr GdkDisplayManager -> IO (Ptr GdkDisplay)

gdkDisplayManagerGetDefaultDisplay :: GdkDisplayManager -> IO GdkDisplay
gdkDisplayManagerGetDefaultDisplay (GdkDisplayManager pdm) = (GdkDisplay <$>)
	$ c_gdk_display_manager_get_default_display pdm >>= \case
		NullPtr -> throw GdkNoDefaultDisplay; pd -> pure pd
	

foreign import ccall "gdk_display_manager_set_default_display" c_gdk_display_manager_set_default_display ::
	Ptr GdkDisplayManager -> Ptr GdkDisplay -> IO ()

gdkDisplayManagerSetDefaultDisplay :: GdkDisplayManager -> GdkDisplay -> IO ()
gdkDisplayManagerSetDefaultDisplay (GdkDisplayManager dm) (GdkDisplay d) =
	c_gdk_display_manager_set_default_display dm d

foreign import ccall "gdk_display_manager_list_displays" c_gdk_display_manager_list_displays ::
	Ptr GdkDisplayManager -> IO (Ptr (GSList GdkDisplay))

gdkDisplayManagerListDisplays :: GdkDisplayManager -> IO [GdkDisplay]
gdkDisplayManagerListDisplays (GdkDisplayManager p) = map GdkDisplay <$> do
	lst <- c_gdk_display_manager_list_displays p
	g_slist_to_list' lst

foreign import ccall "gdk_display_manager_open_display" c_gdk_display_manager_open_display ::
	Ptr GdkDisplayManager -> CString -> IO (Ptr GdkDisplay)

gdkDisplayManagerOpenDisplay :: GdkDisplayManager -> String -> IO GdkDisplay
gdkDisplayManagerOpenDisplay (GdkDisplayManager pdm) n = GdkDisplay
	<$> withCString n \cn ->
		c_gdk_display_manager_open_display pdm cn >>= \case
			NullPtr -> throw GdkCannotOpenDisplay; pd -> pure pd
