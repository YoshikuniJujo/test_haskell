{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.KeySym

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice.Internal
import Graphics.Gdk.Cursors
import Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures

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
	mapM_ (printGdkDevice @_ @'Pointer) =<< gdkSeatGetSlaves st GdkSeatCapabilityAll
	mapM_ (printGdkDevice @_ @'Keyboard) =<< gdkSeatGetSlaves st GdkSeatCapabilityAll
	win <- gdkToplevelNew Nothing defaultGdkWindowAttr
	win2 <- gdkToplevelNew Nothing defaultGdkWindowAttr
	gdkWindowShow win
	gdkWindowShow win2
	gdkDisplayFlush dpy
	gmb <- gdkCursorNewForDisplay dpy GdkGumby
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		e_@(GdkEventGdkKeyPress e) -> gdkEventKey e >>= \k -> case gdkEventKeyKeyval k of
			Xk_q -> pure False
			Xk_s -> True <$ (print =<< gdkSeatGrabSimple st win)
			Xk_o -> True <$ (print =<< gdkSeatGrab st win
						GdkSeatCapabilityAll
						True Nothing Nothing noGdkSeatGrabPrepare)
			Xk_c -> True <$ (print =<< gdkSeatGrab st win
						GdkSeatCapabilityAll
						False (Just gmb) Nothing noGdkSeatGrabPrepare)
			Xk_e -> True <$ (print =<< gdkSeatGrab st win
						GdkSeatCapabilityAll
						False Nothing (Just e_) noGdkSeatGrabPrepare)
			Xk_p -> True <$ (print =<< gdkSeatGrab st win
						GdkSeatCapabilityAll
						False Nothing Nothing (Just (fun, 123)))
			Xk_u -> True <$ gdkSeatUngrab st
			Xk_h -> True <$ gdkWindowHide win
			_ -> True <$ print k
		GdkEventGdkAny e_ -> True <$ do
			e <- gdkEventAny e_
			print e
			print $ gdkEventAnyWindow e

fun :: GdkSeatGrabPrepareFunc Int
fun st wn n = do
	print st
	print wn
	print n
	gdkWindowShow wn

printGdkDevice :: IsGdkDevice d => d pk -> IO ()
printGdkDevice d = do
	n <- gdkDeviceGetName d
	t <- gdkDeviceGetDeviceType d
	s <- gdkDeviceGetSource d
	{-
	mvp <- case t of
		GdkDeviceTypeMaster -> pure Nothing
		_ -> do	mv <- gdkDeviceGetVendorId d
			mp <- gdkDeviceGetProductId d
			pure $ (,) <$> mv <*> mp
	-}
	putStrLn n
	putStrLn $ "\tDeviceType: " ++ show t
	putStrLn $ "\tSource    : " ++ show s
	{-
	case mvp of
		Nothing -> pure ()
		Just (v, p) -> do
			putStrLn $ "\tVendorId  : " ++ v
			putStrLn $ "\tProductId : " ++ p
	-}
