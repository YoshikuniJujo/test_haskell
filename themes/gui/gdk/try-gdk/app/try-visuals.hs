{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.Visuals

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	let	scr = gdkDisplayGetDefaultScreen dpy
	vs <- gdkScreenGetSystemVisual scr
	Just va <- gdkScreenGetRgbaVisual scr
	print $ gdkVisualGetScreen vs
	print $ gdkVisualGetScreen va
	print $ gdkVisualGetVisualType vs
	print $ gdkVisualGetVisualType va
	print $ gdkVisualGetDepth vs
	print $ gdkVisualGetDepth va
