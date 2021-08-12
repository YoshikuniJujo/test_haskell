{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Cursors
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events

import Try.Tools.DoWhile

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	c0 <- gdkCursorNewForDisplay dpy GdkArrow
	print dpy
	print $ gdkCursorGetDisplay c0
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits []) 100 100 GdkInputOutput GdkWindowToplevel
	gdkWindowShow w
	gdkWindowSetCursor w c0

	doWhile_ $ gdkWithEventGet $ pure . maybe False (const True)

	gdkDisplayFlush dpy
	void getLine
