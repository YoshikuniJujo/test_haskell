{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	wr <- gdkScreenGetRootWindow $ gdkDisplayGetDefaultScreen dpy
	w0 <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [])
		900 700 GdkInputOutput GdkWindowToplevel
	w1 <- gdkWindowNew (Just wr) $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [])
		900 700 GdkInputOutput GdkWindowToplevel
	gdkWindowShow w0
	gdkWindowShow w1
	gdkDisplayFlush dpy
	threadDelay 1000000
	gdkWindowDestroy w0
	gdkDisplayFlush dpy
	threadDelay 1000000
