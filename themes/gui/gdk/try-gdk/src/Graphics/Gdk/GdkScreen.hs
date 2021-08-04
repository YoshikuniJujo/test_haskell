{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkScreen (
	-- * TYPE
	GdkScreen,

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

import Graphics.Gdk.GdkScreen.Internal
