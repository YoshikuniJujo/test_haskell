{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Maybe

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice

main :: IO ()
main = do
	dpy <- gdkDisplayOpen ""
	st <- gdkDisplayGetDefaultSeat dpy
	print st
	print dpy
	print =<< gdkSeatGetDisplay st
	print . gdkSeatCapabilityList =<< gdkSeatGetCapabilities st
	printGdkDevice =<< gdkSeatGetPointer st
	printGdkDevice =<< gdkSeatGetKeyboard st

printGdkDevice :: GdkDevice -> IO ()
printGdkDevice d = do
	n <- gdkDeviceGetName d
	t <- gdkDeviceGetDeviceType d
	s <- gdkDeviceGetSource d
	mvp <- case t of
		GdkDeviceTypeMaster -> pure Nothing
		_ -> do	mv <- gdkDeviceGetVendorId d
			mp <- gdkDeviceGetProductId d
			pure $ (,) <$> mv <*> mp
	putStrLn n
	putStrLn $ "\tDeviceType: " ++ show t
	putStrLn $ "\tSource    : " ++ show s
	case mvp of
		Nothing -> pure ()
		Just (v, p) -> do
			putStrLn $ "\tVendorId  : " ++ v
			putStrLn $ "\tProductId : " ++ p
