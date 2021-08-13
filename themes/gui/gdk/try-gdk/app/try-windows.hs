{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits []) 900 700 GdkInputOutput GdkWindowToplevel
	gdkWindowShow w
	gdkDisplayFlush dpy
	threadDelay 2000000
