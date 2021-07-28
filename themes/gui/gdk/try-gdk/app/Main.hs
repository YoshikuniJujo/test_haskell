{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Arrow
import Control.Monad
import Control.Concurrent
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.List
import Data.Char
import Data.IORef
import System.Environment
import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplayManager
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkMonitor
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.GdkDevice.GdkAxes
import Graphics.Gdk.PointsAndRectangles
import Graphics.Gdk.Visuals
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.GdkDrawingContext
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Regions
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Utilities.Types

import Data.Bool

import Data.Color

main :: IO ()
main = do
	opacity <- newIORef 1.0
	pos <- newIORef 0
	size <- newIORef 0
	pn <- getProgName
	as <- getArgs
	gdkSetAllowedBackends "win32,x11,*"
	(_pn', as') <- gdkInit pn as
	print as'
	print =<< gdkGetShowEvents
	print =<< gdkGetDisplayArgName
	print =<< gdkGetProgramClass
	gdkSetProgramClass "Foo"
	print =<< gdkGetProgramClass
	putStrLn . gdkDisplayGetName =<< gdkDisplayManagerGetDefaultDisplay =<< gdkDisplayManagerGet
	print . (gdkDisplayGetName <$>) =<< gdkDisplayManagerListDisplays =<< gdkDisplayManagerGet
	d <- gdkDisplayGetDefault
	putStr "gdkDisplaySupportsSelectionNotification: "
	print =<< gdkDisplaySupportsSelectionNotification d
	putStr "gdkDisplaySupportsClipboarPersistence: "
	print =<< gdkDisplaySupportsClipboardPersistence d
	putStrLn "gdkScreenGetResolution #1"
	print =<< gdkScreenGetResolution (gdkDisplayGetDefaultScreen d)
	st0 <- gdkDisplayGetDefaultSeat d
	[st] <- gdkDisplayListSeats d
	print st0
	print st

	checkGrabbedPointerKeyboard d st

	slvs <- gdkSeatGetSlaves st GdkSeatCapabilityAll
	putStrLn "Slave devices:"
	for_ slvs \slv -> do
		putStrLn . ("\t" ++) . show =<< gdkDeviceGetDeviceType slv
		putStrLn . ("\t" ++) =<< gdkDeviceGetName slv
		putStrLn . ("\t\t" ++) . show =<< gdkDeviceGetVendorId slv
		putStrLn . ("\t\t" ++) . show =<< gdkDeviceGetProductId slv
		s <- gdkDeviceGetSource slv
		putStrLn $ "\t\t" ++ show s
		when (s /= GdkSourceKeyboard) do
			n <- gdkDeviceGetNAxes slv
			putStrLn $ "\t\t" ++ show n
			putStrLn . ("\t\t" ++) . show =<< mapM gdkAtomName =<< gdkDeviceListAxes slv
			putStrLn . ("\t\t" ++) . show . gdkAxisFlagList =<< gdkDeviceGetAxes slv
			for_ [0 .. fromIntegral n - 1] \i ->
				putStrLn . ("\t\t" ++) . show =<< gdkDeviceGetAxisUse slv i
	gdkDisplayGetPrimaryMonitor d >>= \case
		Nothing -> putStrLn "no primary monitor"
		Just mntr -> do
			print mntr
			print =<< gdkMonitorGetManufacturer mntr
			print =<< gdkMonitorGetModel mntr
			putStrLn . ("Monitor geometry: " ++) . show =<< gdkMonitorGetGeometry mntr
			putStrLn . ("Monitor workarea: " ++) . show =<< gdkMonitorGetWorkarea mntr
			putStrLn . ("Monitor width: " ++) . show =<< gdkMonitorGetWidthMm mntr
			putStrLn . ("Monitor height: " ++) . show =<< gdkMonitorGetHeightMm mntr
			putStrLn . ("Monitor scale factor: " ++) . show =<< gdkMonitorGetScaleFactor mntr
			putStrLn . ("Monitor refresh rate: " ++) . show =<< gdkMonitorGetRefreshRate mntr
			putStrLn . ("Monitor subpixel layout: " ++) . show =<< gdkMonitorGetSubpixelLayout mntr
			putStrLn . ("Monitor is primary: " ++) . show =<< gdkMonitorIsPrimary mntr
	putStrLn . ("Supports cursor color: " ++) . show =<< gdkDisplaySupportsCursorColor d
	putStrLn . ("Supports cursor alpha: " ++) . show =<< gdkDisplaySupportsCursorAlpha d
	putStrLn . ("Default cursor size: " ++) . show =<< gdkDisplayGetDefaultCursorSize d
	putStrLn . ("Number of monitors: " ++) . show =<< gdkDisplayGetNMonitors d
	Just scrn <- gdkScreenGetDefault
	vsl <- gdkScreenGetSystemVisual scrn
	putStrLn . ("Depth of system visual: " ++) . show =<< gdkVisualGetDepth vsl
	maybe (putStrLn "No rgba visual") (\v -> putStrLn . ("Depth of rgba visual: " ++) . show =<< gdkVisualGetDepth v)
		=<< gdkScreenGetRgbaVisual scrn
	putStrLn . ("Screen is composited: " ++) . show =<< gdkScreenIsComposited scrn
	rtwn <- gdkScreenGetRootWindow scrn
	putStrLn . ("Window type of root window: " ++) . show =<< gdkWindowGetWindowType rtwn
	putStrLn . ("State of root window: " ++) . show . gdkWindowStateList =<< gdkWindowGetState rtwn
	gdkScreenListVisuals scrn >>= \case
		vs -> do
		--	print vs
			ds <- for vs gdkVisualGetDepth
			putStrLn $ "Depth of visuals: " ++ show ((head &&& length) <$> group ds)
			ts <- for vs gdkVisualGetVisualType
			putStrLn $ "Types of visuals: " ++ show ((head &&& length) <$> group ts)
			rds <- for vs gdkVisualGetRedPixelDetails
			putStrLn $ "Red pixel details of visuals: " ++ show ((head &&& length) <$> group rds)
			grs <- for vs gdkVisualGetGreenPixelDetails
			putStrLn $ "Green pixel details of visuals: " ++ show ((head &&& length) <$> group grs)
			bls <- for vs gdkVisualGetBluePixelDetails
			putStrLn $ "Blue pixel details of visuals: " ++ show ((head &&& length) <$> group bls)
	putStrLn "gdkScreenGetToplevelWindows #1"
	gdkScreenGetToplevelWindows scrn >>= \case
		tws -> for_ tws \tw -> print =<< gdkWindowGetWindowType tw
	putStrLn "gdkScreenGetWindowStack #1"
	gdkScreenGetWindowStack scrn >>=
		mapM_ \tw -> print =<< withGdkWindowAutoUnref tw gdkWindowGetWindowType
	print =<< gdkSeatGetCapabilities st
	let wattr = minimalGdkWindowAttr (gdkEventMaskMultiBits [
				GdkExposureMask, GdkButtonPressMask, GdkKeyPressMask, GdkFocusChangeMask,
				GdkEnterNotifyMask, GdkLeaveNotifyMask, GdkPointerMotionMask,
--				GdkAllEventsMask,
				GdkPointerMotionMask
				])
			400 400
			GdkInputOutput GdkWindowToplevel
	putStrLn "*** GDK WINDOW NEW ***"
	w <- gdkWindowNew Nothing wattr { gdkWindowAttrTitle = Just "試験窓" }
	printVisibleRegion w
	print =<< gdkWindowGetWindowType w
	putStr "PARENT WINDOW: "
	print =<< gdkWindowGetWindowType =<< gdkWindowGetParent w
	putStr "THIS WINDOW: "
	print w
	putStr "TOPLEVEL WINDOW: "
	print =<< gdkWindowGetToplevel w
	putStr "CHILDREN OF THIS WINDOW: "
	print =<< gdkWindowPeekChildren w
	putStr "CHILDREN OF PARENT WINDOW: "
	print =<< mapM gdkWindowGetWindowType =<< gdkWindowPeekChildren =<< gdkWindowGetParent w
	printVisual =<< gdkWindowGetVisual w
	print =<< gdkWindowGetDecorations w
	print GdkWindowToplevel
	print GdkWindowRoot
	putStrLn . gdkDisplayGetName =<< gdkWindowGetDisplay w
	putStrLn "gdkScreenGetResolution #2"
	print =<< gdkScreenGetResolution =<< gdkWindowGetScreen w
	print =<< gdkVisualGetDepth =<< gdkWindowGetVisual w
	putStrLn . ("Window is destroyed: " ++) . show =<< gdkWindowIsDestroyed w
	putStrLn . ("Window is visible: " ++) . show =<< gdkWindowIsVisible w
	putStrLn . ("Window is viewable: " ++) . show =<< gdkWindowIsViewable w
	putStrLn . ("Window is input only: " ++) . show =<< gdkWindowIsInputOnly w
	putStrLn . ("Window is shaped: " ++) . show =<< gdkWindowIsShaped w
	putStrLn . ("Window state: " ++) . show . gdkWindowStateList =<< gdkWindowGetState w
	putStr "GDK WINDOW GET EVENTS: "
	print . gdkEventMaskSingleBitList =<< gdkWindowGetEvents w
	gdkWindowShow w
	printVisibleRegion w
	gdkWindowSetOpacity w 0.5
	putStrLn . ("Window is visible: " ++) . show =<< gdkWindowIsVisible w
	putStrLn . ("Window is viewable: " ++) . show =<< gdkWindowIsViewable w
	putStrLn . ("Window state: " ++) . show . gdkWindowStateList =<< gdkWindowGetState w
	gdkWindowSetEvents w $ gdkEventMaskMultiBits [
		GdkExposureMask, GdkButtonPressMask, GdkFocusChangeMask, GdkKeyPressMask,
		GdkPointerMotionMask ] -- , gdkAllEventsMask ]
	print GdkExposureMask
	print GdkPointerMotionMask
	putStrLn "gdkScreenGetTopLevelWindows #2"
	gdkScreenGetToplevelWindows scrn >>=
		mapM_ \tw -> print =<< gdkWindowGetWindowType tw
	putStrLn "gdkScreenGetWindowStack #2"
	gdkScreenGetWindowStack scrn >>=
		mapM_ \tw -> print =<< withGdkWindowAutoUnref tw gdkWindowGetWindowType
	checkGrabbedPointerKeyboard d st
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkWithEventGet \case
			Just e -> do
				b <- checkEventSealed opacity pos size d st e
				pure if b then Nothing else Just False
			Nothing -> pure $ Just True
	gdkWindowDestroy w
	putStrLn . ("Window is destroyed: " ++) . show =<< gdkWindowIsDestroyed w
	putStrLn . ("Window is visible: " ++) . show =<< gdkWindowIsVisible w
	putStrLn . ("Window is viewable: " ++) . show =<< gdkWindowIsViewable w

	putStrLn "CLOSE DISPLAY"
	print =<< gdkDisplayIsClosed d
	gdkDisplayClose d
--	print =<< gdkDisplayIsClosed d

checkGrabbedPointerKeyboard :: GdkDisplay -> GdkSeat -> IO ()
checkGrabbedPointerKeyboard d st = do
	putStrLn "\nPOINTER:"
	pnt <- gdkSeatGetPointer st
	putStrLn =<< gdkDeviceGetName pnt
	print =<< gdkDisplayDeviceIsGrabbed d pnt
	putStrLn "KEYBOARD"
	kbd <- gdkSeatGetKeyboard st
	putStrLn =<< gdkDeviceGetName kbd
	print =<< gdkDisplayDeviceIsGrabbed d kbd
	putStrLn ""

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = bool (pure ()) (doWhile_ act) =<< act

doWhile :: Monad m => m (Maybe a) -> m a
doWhile act = maybe (doWhile act) pure =<< act
		
printVisual :: GdkVisual -> IO ()
printVisual v = do
	d <- gdkVisualGetDepth v
	putStrLn $ "Depth of visual: " ++ show d
	t <- gdkVisualGetVisualType v
	putStrLn $ "Types of visual: " ++ show t
	rd <- gdkVisualGetRedPixelDetails v
	putStrLn $ "Red pixel details of visual: " ++ show rd
	gr <- gdkVisualGetGreenPixelDetails v
	putStrLn $ "Green pixel details of visual: " ++ show gr
	bl <- gdkVisualGetBluePixelDetails v
	putStrLn $ "Blue pixel details of visual: " ++ show bl

printVisibleRegion :: GdkWindow -> IO ()
printVisibleRegion w = do
	vr <- gdkWindowGetVisibleRegion w
	print vr
	print =<< cairoRegionNumRectangles vr
	vrrp <- cairoRectangleIntTNew
	cairoRegionGetRectangle vr 0 vrrp
	print =<< cairoRectangleIntTFreeze vrrp

checkKeyVal :: Char -> GdkKeySym -> Bool
checkKeyVal c ks = ks == GdkKeySym (fromIntegral $ ord c)

checkEventSealed :: IORef CDouble -> IORef Int -> IORef Int -> GdkDisplay -> GdkSeat -> GdkEvent s -> IO Bool
checkEventSealed opacity pos size d st = \case
	GdkEventGdkNothing n -> do
		putStrLn $ "GDK_NOTHING: " ++ show n
		pure True
	GdkEventGdkDelete dl -> do
		putStrLn $ "GDK_DELETE: " ++ show dl
		pure False
	GdkEventGdkKeyPress k_ -> do
		let	k = gdkEventKey k_
			w = gdkEventKeyWindow k
			kv = gdkEventKeyKeyval k
		putStrLn $ "GDK_KEY_PRESS: " ++ show k ++ ": " ++ show kv
		putStrLn $ "GdkModifierType: " ++ show (gdkEventKeyState k)
		when (checkKeyVal 'h' kv) $ do
			putStrLn "`h' pressed"
			gdkWindowHide w
			void . forkIO $ do
				threadDelay 3000000
				gdkWindowShow w
		when (checkKeyVal 'i' kv) $ do
			putStrLn "`i' pressed"
			gdkWindowIconify w
			print . gdkWindowStateList =<< gdkWindowGetState w
			void . forkIO $ do
				threadDelay 1000000
				print . gdkWindowStateList =<< gdkWindowGetState w
				gdkWindowDeiconify w
				print . gdkWindowStateList =<< gdkWindowGetState w
				threadDelay 1000000
				print . gdkWindowStateList =<< gdkWindowGetState w
		when (checkKeyVal 'j' kv) $ do
			putStrLn "'j' pressed"
			s <- gdkWindowGetState w
			if gdkWindowStateCheck GdkWindowStateSticky s
				then gdkWindowUnstick w
				else gdkWindowStick w
			print . gdkWindowStateList =<< gdkWindowGetState w
			void . forkIO $ do
				threadDelay 1000000
				print . gdkWindowStateList =<< gdkWindowGetState w
		when (checkKeyVal 'm' kv) $ do
			putStrLn "`m' pressed"
			s <- gdkWindowGetState w
			if gdkWindowStateCheck GdkWindowStateMaximized s
				then gdkWindowUnmaximize w
				else gdkWindowMaximize w
			void . forkIO $ do
				threadDelay 1000000
				print . gdkWindowStateList =<< gdkWindowGetState w
		when (checkKeyVal 'f' kv) $ do
			putStrLn "`f' pressed"
			s <- gdkWindowGetState w
			if gdkWindowStateCheck GdkWindowStateFullscreen s
				then gdkWindowUnfullscreen w
				else gdkWindowFullscreen w
			void . forkIO $ do
				threadDelay 1000000
				print . gdkWindowStateList =<< gdkWindowGetState w
		when (checkKeyVal 'a' kv) $ do
			putStrLn "`a' pressed"
			s <- gdkWindowGetState w
			gdkWindowSetKeepAbove w . not $ gdkWindowStateCheck GdkWindowStateAbove s
			void . forkIO $ do
				threadDelay 1000000
				print . gdkWindowStateList =<< gdkWindowGetState w
		when (checkKeyVal 'b' kv) $ do
			putStrLn "`b' pressed"
			s <- gdkWindowGetState w
			gdkWindowSetKeepBelow w . not $ gdkWindowStateCheck GdkWindowStateBelow s
			void . forkIO $ do
				threadDelay 1000000
				print . gdkWindowStateList =<< gdkWindowGetState w
		when (checkKeyVal 'o' kv) $ do
			modifyIORef opacity (snd . properFraction @_ @Int . (+ 0.0625))
			gdkWindowSetOpacity w =<< readIORef opacity
			print =<< readIORef opacity
		when (checkKeyVal 't' kv) $ do
			modifyIORef pos (+ 1)
			p <- readIORef pos
			uncurry (gdkWindowMove w) $ case p `mod` 8 of
				0 -> (100, 100)
				1 -> (500, 100)
				2 -> (900, 100)
				3 -> (900, 300)
				4 -> (900, 500)
				5 -> (500, 500)
				6 -> (100, 500)
				7 -> (100, 300)
				_ -> error "never occur"
		when (checkKeyVal 'u' kv) $ do
			modifyIORef size (+ 1)
			s <- readIORef size
			uncurry (gdkWindowResize w) $ case s `mod` 8 of
				0 -> (100, 100)
				1 -> (500, 100)
				2 -> (900, 100)
				3 -> (900, 300)
				4 -> (900, 500)
				5 -> (500, 500)
				6 -> (100, 500)
				7 -> (100, 300)
				_ -> error "never occur"
		when (checkKeyVal 'v' kv) $ do
			gdkWindowLower w
			void . forkIO $ threadDelay 2000000 >> gdkWindowRaise w >> gdkWindowFocus w 0
		when (checkKeyVal 'p' kv) $ do
			putStrLn . ("Window size: " ++) . show =<< gdkWindowGetPosition w
		when (checkKeyVal 's' kv) $ do
			putStrLn . ("Window size: " ++) . show =<< (,) <$> gdkWindowGetWidth w <*> gdkWindowGetHeight w
		when (checkKeyVal 'g' kv) $ checkGrabbedPointerKeyboard d st
		when (checkKeyVal 'w' kv) do
			pnt <- gdkSeatGetPointer st
			gdkDeviceWarp pnt (gdkDisplayGetDefaultScreen d) 100 100
		when (checkKeyVal 'r' kv) do
			pnt <- gdkSeatGetPointer st
			print =<< gdkDeviceGetPosition pnt
			print =<< gdkDeviceGetPositionDouble pnt
			print =<< gdkDeviceGetWindowAtPosition pnt
			print =<< gdkDeviceGetWindowAtPositionDouble pnt
		when (checkKeyVal 'c' kv) do
			putStrLn "`c' pressed!"
			putStr "gdkWindowGetGeometry: "
			print =<< gdkWindowGetGeometry w
			putStr "gdkWindowGet{Width|Height}: "
			print =<< (,)
				<$> gdkWindowGetWidth w <*> gdkWindowGetHeight w
			putStr "gdkWindowGetPosition: "
			print =<< gdkWindowGetPosition w
			putStr "gdkWindowGetRootOrigin: "
			print =<< gdkWindowGetRootOrigin w
			putStr "gdkWindowGetFrameExtents: "
			r <- gdkRectangleNew
			gdkWindowGetFrameExtents w r
			print =<< gdkRectangleFreeze r
			putStr "gdkWindowGetOrigin: "
			print =<< gdkWindowGetOrigin w
			putStr "gdkWindowGetRootCoords w 100 200: "
			print =<< gdkWindowGetRootCoords w 100 200
		when (checkKeyVal 'd' kv) do
			putStrLn "`d' pressed"
			w' <- gdkWindowNew Nothing $ minimalGdkWindowAttr
				GdkZeroEventsMask 100 100 GdkInputOutput GdkWindowToplevel
			gdkWindowSetTransientFor w' w
			gdkWindowSetModalHint w' True
			gdkWindowShow w'
		when (checkKeyVal 'e' kv) do
			putStrLn "`e' pressed"
			gdkWindowSetSkipTaskbarHint w True
		when (checkKeyVal 'k' kv) do
			putStrLn "`k' pressed"
			gdkWindowSetSkipTaskbarHint w False
		when (checkKeyVal 'l' kv) do
			putStrLn "`l' pressed"
			gdkWindowSetSkipPagerHint w True
		when (checkKeyVal 'n' kv) do
			putStrLn "`n' pressed"
			gdkWindowSetSkipPagerHint w False
		when (checkKeyVal '1' kv) do
			putStrLn "`1' pressed"
			gdkWindowSetUrgencyHint w True
		when (checkKeyVal '2' kv) do
			putStrLn "`2' pressed"
			gdkWindowSetUrgencyHint w False
		pure . not $ checkKeyVal 'q' kv
	GdkEventGdkKeyRelease k -> True <$ print k
	GdkEventGdkFocusChange f -> True <$ print f
	GdkEventGdkMap m -> do
		putStrLn $ "GDK_MAP: " ++ show m
		pure True
	GdkEventGdkUnmap m -> do
		putStrLn $ "GDK_UNMAP: " ++ show m
		pure True
	GdkEventGdkConfigure c -> True <$ print c
	GdkEventGdkVisibilityNotify (gdkEventVisibility -> v) -> do
		print v
		let	w = gdkEventVisibilityWindow v
		r <- cairoRegionCreateRectangle $ CairoRectangleIntT 50 50 100 100
		gdkWindowWithDrawFrame w r \cxt -> do
			cr <- gdkDrawingContextGetCairoContext cxt
			cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.2
			cairoSetLineWidth cr 5
			cairoMoveTo cr 10 10
			cairoLineTo cr 90 90
			cairoStroke cr
		pure True
	GdkEventGdkWindowState s -> True <$ print s
	GdkEventGdkMotionNotify m -> True <$ print m
	GdkEventGdkAny a -> True <$ print a
