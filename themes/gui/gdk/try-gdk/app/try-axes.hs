{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.GdkDevice.GdkAxes

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	st <- gdkDisplayGetDefaultSeat dpy

	pnt <- gdkSeatGetPointer st
	kbd <- gdkSeatGetKeyboard st
	
	pnts <- gdkDeviceListSlaveDevices pnt
	kbds <- gdkDeviceListSlaveDevices kbd

	printAxis pnt
	printAxis `mapM_` pnts

printAxis :: IsGdkDevice d => d 'Pointer -> IO ()
printAxis d = do
	putStrLn =<< gdkDeviceGetName d
	putStrLn . ("\tgdkDeviceGetNAxes: " ++) . show =<< gdkDeviceGetNAxes d
	putStrLn ""
