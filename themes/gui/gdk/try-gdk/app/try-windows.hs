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
	let	scr = gdkDisplayGetDefaultScreen dpy
	wr <- gdkScreenGetRootWindow scr
	w0 <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [])
		900 700 GdkInputOutput GdkWindowToplevel
	w1 <- gdkWindowNew (Just wr) $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [])
		900 700 GdkInputOutput GdkWindowToplevel
	print dpy
	print $ gdkWindowGetDisplay w0
	print scr
	print $ gdkWindowGetScreen w0
	print =<< gdkScreenGetSystemVisual scr
	print =<< gdkScreenGetRgbaVisual scr
	print $ gdkWindowGetVisual w0

	print =<< gdkScreenGetRootWindow scr
	print =<< gdkGetDefaultRootWindow

	gdkWindowShow w0
	gdkDisplayFlush dpy
	threadDelay 1000000
	gdkWindowShow w1
	gdkDisplayFlush dpy
	threadDelay 1000000
	gdkWindowHide w0
	gdkDisplayFlush dpy
	threadDelay 1000000
	gdkWindowLower w0
	gdkWindowShowUnraised w0
	gdkDisplayFlush dpy
	threadDelay 1000000
	gdkWindowShow w0
	gdkDisplayFlush dpy
	threadDelay 1000000
	gdkWindowDestroy w0
	gdkDisplayFlush dpy
	threadDelay 1000000
