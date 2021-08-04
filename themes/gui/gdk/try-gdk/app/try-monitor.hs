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

	putStrLn "\nMANUFACTURER AND MODEL"
	putStr "gdkMonitorGetManufacturer: "
	print =<< gdkMonitorGetManufacturer mnt
	putStr "gdkMonitorGetModel       : "
	print =<< gdkMonitorGetModel mnt

	putStrLn "\nPROPERTIES"
	putStr "gdkMonitorGetScaleFactor  : "
	print =<< gdkMonitorGetScaleFactor mnt
	putStr "gdkMonitorGetRefreshRate  : "
	print =<< gdkMonitorGetRefreshRate mnt
	putStr "gdkMonitorGetSubpixelLyout: "
	print =<< gdkMonitorGetSubpixelLayout mnt

	putStrLn "\nIS PRIMARY"
	putStr "gdkMonitorIsPrimary: "
	print =<< gdkMonitorIsPrimary mnt
