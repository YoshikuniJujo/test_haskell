{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkScreen.Internal (
	-- * TYPE
	GdkScreen(..),

	-- * DEFAULT SCREEN
	gdkScreenGetDefault,
	-- * VISUAL
	gdkScreenGetSystemVisual,
	gdkScreenGetRgbaVisual,
	gdkScreenListVisuals,

	-- * IS COMPOSITED
	gdkScreenIsComposited,

	-- * WINDOW
	gdkScreenGetRootWindow,
	gdkScreenGetToplevelWindows,
	gdkScreenGetWindowStack,

	-- * DISPLAY
	gdkScreenGetDisplay ) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Data.Int
import System.GLib.Bool
import System.GLib.DoublyLinkedLists

import {-# SOURCE #-} Graphics.Gdk.GdkDisplay.Internal
import Graphics.Gdk.Windows.Internal
import Graphics.Gdk.Visuals.Internal

#include <gdk/gdk.h>

newtype GdkScreen = GdkScreen (Ptr GdkScreen) deriving Show

gdkScreenGetDefault :: IO (Maybe GdkScreen)
gdkScreenGetDefault = (<$> c_gdk_screen_get_default) \case
	NullPtr -> Nothing; p -> Just $ GdkScreen p

foreign import ccall "gdk_screen_get_default" c_gdk_screen_get_default ::
	IO (Ptr GdkScreen)

gdkScreenGetSystemVisual :: GdkScreen -> IO GdkVisual
gdkScreenGetSystemVisual (GdkScreen p) = GdkVisual <$> c_gdk_screen_get_system_visual p

foreign import ccall "gdk_screen_get_system_visual" c_gdk_screen_get_system_visual ::
	Ptr GdkScreen -> IO (Ptr GdkVisual)

gdkScreenGetRgbaVisual :: GdkScreen -> IO (Maybe GdkVisual)
gdkScreenGetRgbaVisual (GdkScreen s) = (<$> c_gdk_screen_get_rgba_visual s) \case
	v	| v == nullPtr -> Nothing
		| otherwise -> Just $ GdkVisual v

foreign import ccall "gdk_screen_get_rgba_visual" c_gdk_screen_get_rgba_visual ::
	Ptr GdkScreen -> IO (Ptr GdkVisual)

gdkScreenIsComposited :: GdkScreen -> IO Bool
gdkScreenIsComposited (GdkScreen s) = gbooleanToBool <$> c_gdk_screen_is_composited s

foreign import ccall "gdk_screen_is_composited" c_gdk_screen_is_composited ::
	Ptr GdkScreen -> IO #type gboolean

gdkScreenGetRootWindow :: GdkScreen -> IO GdkWindow
gdkScreenGetRootWindow (GdkScreen p) = GdkWindow <$> c_gdk_screen_get_root_window p

foreign import ccall "gdk_screen_get_root_window" c_gdk_screen_get_root_window ::
	Ptr GdkScreen -> IO (Ptr GdkWindow)

gdkScreenGetDisplay :: GdkScreen -> IO GdkDisplay
gdkScreenGetDisplay (GdkScreen p) = GdkDisplay <$> c_gdk_screen_get_display p

foreign import ccall "gdk_screen_get_display" c_gdk_screen_get_display ::
	Ptr GdkScreen -> IO (Ptr GdkDisplay)

gdkScreenListVisuals :: GdkScreen -> IO [GdkVisual]
gdkScreenListVisuals (GdkScreen p) = do
	gl <- c_gdk_screen_list_visuals p
	maybe [] (map GdkVisual) <$> g_list_to_list gl <* c_g_list_free gl

foreign import ccall "gdk_screen_list_visuals" c_gdk_screen_list_visuals ::
	Ptr GdkScreen -> IO (Ptr (GList GdkVisual))

gdkScreenGetToplevelWindows :: GdkScreen -> IO [GdkWindow]
gdkScreenGetToplevelWindows (GdkScreen p) = do
	gl <- c_gdk_screen_get_toplevel_windows p
	maybe [] (map GdkWindow) <$> g_list_to_list gl <* c_g_list_free gl

foreign import ccall "gdk_screen_get_toplevel_windows" c_gdk_screen_get_toplevel_windows ::
	Ptr GdkScreen -> IO (Ptr (GList GdkWindow))

gdkScreenGetWindowStack :: GdkScreen -> IO (Maybe [GdkWindowAutoUnref])
gdkScreenGetWindowStack (GdkScreen p) = do
	gl <- c_gdk_screen_get_window_stack p
	maybe (pure Nothing) ((Just <$>) . mapM mkGdkWindowAutoUnref) =<< g_list_to_list gl <* c_g_list_free gl

foreign import ccall "gdk_screen_get_window_stack" c_gdk_screen_get_window_stack ::
	Ptr GdkScreen -> IO (Ptr (GList GdkWindowNeedUnref))
