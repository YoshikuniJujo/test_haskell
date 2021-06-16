{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkScreen where

import Foreign.Ptr
import Control.Arrow
import Data.Int
import System.GLib.Bool
import System.GLib.DoublyLinkedLists

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

gdkScreenIsComposited :: GdkScreen -> IO Bool
gdkScreenIsComposited (GdkScreen s) = gbooleanToBool <$> c_gdk_screen_is_composited s

foreign import ccall "gdk_screen_get_root_window" c_gdk_screen_get_root_window ::
	Ptr GdkScreen -> IO (Ptr GdkWindow)

gdkScreenGetRootWindow :: GdkScreen -> IO GdkWindow
gdkScreenGetRootWindow (GdkScreen p) = GdkWindow <$> c_gdk_screen_get_root_window p

foreign import ccall "gdk_screen_get_display" c_gdk_screen_get_display ::
	Ptr GdkScreen -> IO (Ptr GdkDisplay)

gdkScreenGetDisplay :: GdkScreen -> IO GdkDisplay
gdkScreenGetDisplay (GdkScreen p) = GdkDisplay <$> c_gdk_screen_get_display p

foreign import ccall "gdk_screen_list_visuals" c_gdk_screen_list_visuals ::
	Ptr GdkScreen -> IO (Ptr (GList GdkVisual))

gdkScreenListVisuals :: GdkScreen -> IO ([GdkVisual], [GdkVisual])
gdkScreenListVisuals (GdkScreen p) = do
	gl <- c_gdk_screen_list_visuals p
	(map GdkVisual *** map GdkVisual) <$> gListListPtr (GListRef gl)
		<* c_g_list_free gl

foreign import ccall "gdk_screen_get_toplevel_windows" c_gdk_screen_get_toplevel_windows ::
	Ptr GdkScreen -> IO (Ptr (GList GdkWindow))

gdkScreenGetToplevelWindows :: GdkScreen -> IO ([GdkWindow], [GdkWindow])
gdkScreenGetToplevelWindows (GdkScreen p) = do
	gl <- c_gdk_screen_get_toplevel_windows p
	(map GdkWindow *** map GdkWindow) <$> gListListPtr (GListRef gl)
		<* c_g_list_free gl

foreign import ccall "gdk_screen_get_resolution" c_gdk_screen_get_resolution ::
	Ptr GdkScreen -> IO #type gdouble

gdkScreenGetResolution :: GdkScreen -> IO Double
gdkScreenGetResolution (GdkScreen p) = c_gdk_screen_get_resolution p

foreign import ccall "g_list_free" c_g_list_free :: Ptr (GList a) -> IO ()

foreign import ccall "gdk_screen_get_window_stack" c_gdk_screen_get_window_stack ::
	Ptr GdkScreen -> IO (Ptr (GList GdkWindow))

gdkScreenGetWindowStack :: GdkScreen -> IO ([GdkWindow], [GdkWindow])
gdkScreenGetWindowStack (GdkScreen p) = do
	gl <- c_gdk_screen_get_window_stack p
	(map GdkWindow *** map GdkWindow) <$> gListListPtr (GListRef gl)
		<* c_g_list_free gl
