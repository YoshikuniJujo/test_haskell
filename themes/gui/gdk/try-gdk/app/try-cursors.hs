{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Cursors

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	c0 <- gdkCursorNewForDisplay dpy GdkXCursor
	print dpy
	print $ gdkCursorGetDisplay c0
