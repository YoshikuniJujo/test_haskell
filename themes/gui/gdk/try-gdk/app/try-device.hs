{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Foldable
import Data.Maybe
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice.Internal
import Graphics.Gdk.GdkDevice.GdkAxes
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	let	scr = gdkDisplayGetDefaultScreen dpy
	print dpy
	print scr
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

	pnts <- gdkDeviceListSlaveDevices pnt
	kbds <- gdkDeviceListSlaveDevices kbd
	putStrLn =<< gdkDeviceGetName pnt
	for_ pnts \ps -> putStrLn . ('\t' :) =<< gdkDeviceGetName ps
	putStrLn ""
	putStrLn =<< gdkDeviceGetName kbd
	for_ kbds \ks -> putStrLn . ('\t' :) =<< gdkDeviceGetName ks
	putStrLn ""

	printDevice pnt

	gdkDeviceWarp pnt scr 100 100
--	gdkDeviceWarp (toGdkDevice kbd) scr 100 100
--	gdkDeviceWarp (toGdkDevice $ pnts !! 1) scr 100 100
	gdkDisplayFlush dpy

	print =<< gdkDeviceGetLastEventWindow pnt

	print =<< gdkDeviceGetNAxes pnt
	for_ pnts \ps -> print =<< gdkDeviceGetNAxes ps
--	print =<< gdkDeviceGetNAxes kbd
--	for_ kbds \ks -> print =<< gdkDeviceGetNAxes ks

	print =<< mapM gdkAtomName . fromJust =<< gdkDeviceListAxes pnt
	for_ pnts \ps ->
		print =<< mapM gdkAtomName . fromJust =<< gdkDeviceListAxes ps

printDevice :: IsGdkDevice d => d -> IO ()
printDevice d = do
	n <- gdkDeviceGetName d
	t <- gdkDeviceGetDeviceType d
	s <- gdkDeviceGetSource d
	putStrLn n
	putStrLn $ '\t' : show t
	putStrLn $ '\t' : show s

{-
	v <- gdkDeviceGetVendorId d
	p <- gdkDeviceGetProductId d
	putStrLn $ '\t' : show v
	putStrLn $ '\t' : show p
	putStrLn ""
	-}
