{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Foldable
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
	putStrLn ""
	mpnts <- gdkDeviceListSlaveDevices pnt
	mkbds <- gdkDeviceListSlaveDevices kbd
	putStrLn =<< gdkDeviceGetName pnt
	flip (maybe $ pure ()) mpnts \pnts -> for_ pnts \ps -> do
		putStrLn . ('\t' :) =<< gdkDeviceGetName ps
	putStrLn ""
	putStrLn =<< gdkDeviceGetName kbd
	flip (maybe $ pure ()) mkbds \kbds -> for_ kbds \ks -> do
		putStrLn . ('\t' :) =<< gdkDeviceGetName ks
