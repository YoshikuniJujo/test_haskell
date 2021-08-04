{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkMonitor

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	Just mnt <- gdkDisplayGetPrimaryMonitor dpy
	print mnt
	print dpy
	print =<< gdkMonitorGetDisplay mnt
	putStrLn "\nGEOMETRY"
	putStr "gdkMonitorGetGeometry: "
	print =<< gdkMonitorGetGeometry mnt
	putStr "gdkMonitorGetWorkarea: "
	print =<< gdkMonitorGetWorkarea mnt
	putStr "gdkMonitorGetWidthMm : "
	print =<< gdkMonitorGetWidthMm mnt
	putStr "gdkMonitorGetHeightMm: "
	print =<< gdkMonitorGetHeightMm mnt
