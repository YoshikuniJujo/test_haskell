{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Maybe

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Try.Tools

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
	mapM_ printGdkDevice =<< gdkSeatGetSlaves st GdkSeatCapabilityAll
	win <- gdkWindowNew Nothing defaultGdkWindowAttr
	gdkWindowShow win
	gdkDisplayFlush dpy
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
			pure False
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_s) ->
			True <$ (print =<< gdkSeatGrabSimple st win)
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_u) ->
			True <$ gdkSeatUngrab st
		e -> True <$ print e

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
