{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	st <- gdkDisplayGetDefaultSeat dpy
	print st
	print dpy
	print =<< gdkSeatGetDisplay st
