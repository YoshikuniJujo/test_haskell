{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	print dpy
	st <- gdkDisplayGetDefaultSeat dpy
	print st
	pnt <- gdkSeatGetPointer st
	kbd <- gdkSeatGetKeyboard st
	print pnt
	print kbd
	print =<< gdkDeviceGetDisplay pnt
	print =<< gdkDeviceGetDisplay kbd
	print =<< gdkDeviceGetSeat pnt
	print =<< gdkDeviceGetSeat kbd
