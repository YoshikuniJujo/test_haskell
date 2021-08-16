{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	let	scr = gdkDisplayGetDefaultScreen dpy
	st <- gdkDisplayGetDefaultSeat dpy
	pnt <- gdkSeatGetPointer st
	print pnt
	kbd <- gdkSeatGetKeyboard st
	print kbd
	wr <- gdkScreenGetRootWindow scr
	w0 <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [GdkPointerMotionMask])
		900 700 GdkInputOutput GdkWindowToplevel
	w1 <- gdkWindowNew (Just wr) $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [])
		900 700 GdkInputOutput GdkWindowToplevel
	wc <- gdkWindowNew (Just w0) $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [])
		500 300 GdkInputOutput GdkWindowChild

	print dpy
	print $ gdkWindowGetDisplay w0
	print scr
	print $ gdkWindowGetScreen w0
	print =<< gdkScreenGetSystemVisual scr
	print =<< gdkScreenGetRgbaVisual scr
	print $ gdkWindowGetVisual w0

	print =<< gdkScreenGetRootWindow scr
	print =<< gdkGetDefaultRootWindow

	print w0
	print =<< gdkWindowGetToplevel w0

	print wr
	print =<< gdkWindowGetParent w0

	print w0
	print =<< gdkWindowGetParent wc
	gdkWindowReparent wc w1 100 100
	print w1
	print =<< gdkWindowGetParent wc

	print wc
	print =<< gdkWindowPeekChildren w1

	print . gdkEventMaskSingleBitList =<< gdkWindowGetEvents w0
	print . gdkEventMaskSingleBitList =<< gdkWindowGetDeviceEvents w0 pnt
	gdkWindowSetEvents w0 $ gdkEventMaskMultiBits [GdkKeyPressMask]
	print . gdkEventMaskSingleBitList =<< gdkWindowGetEvents w0
	print . gdkEventMaskSingleBitList =<< gdkWindowGetDeviceEvents w0 pnt

	gdkWindowShow w0
	gdkWindowShow wc
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
