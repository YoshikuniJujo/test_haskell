{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Concurrent
import Data.Foldable
import Data.Traversable
import Data.List
import Data.Char
import System.Environment
import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplayManager
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkMonitor
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.Visuals
import Graphics.Gdk.Windows
import Graphics.Gdk.GdkDrawingContext
import Graphics.Gdk.Event
import Graphics.Gdk.Types
import Graphics.Gdk.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Regions
import Graphics.Cairo.Paths
import Graphics.Cairo.Types

import Data.Bool

main :: IO ()
main = do
	pn <- getProgName
	as <- getArgs
	gdkSetAllowedBackends "win32,x11,*"
	print =<< gdkInit pn as
	print =<< gdkGetDisplayArgName
	gdkSetProgramClass "Foo"
	print =<< gdkGetProgramClass
	putStrLn =<< gdkDisplayGetName =<< gdkDisplayManagerGetDefaultDisplay =<< gdkDisplayManagerGet
	print =<< mapM gdkDisplayGetName =<< gdkDisplayManagerListDisplays =<< gdkDisplayManagerGet
	d <- gdkDisplayGetDefault
	print =<< gdkScreenGetResolution =<< gdkDisplayGetDefaultScreen d
--	st <- gdkDisplayGetDefaultSeat d
	([], [st]) <- gdkDisplayListSeats d
	pnt <- gdkSeatGetPointer st
	kbd <- gdkSeatGetKeyboard st
	putStrLn =<< gdkDeviceGetName pnt
	print =<< gdkDisplayDeviceIsGrabbed d pnt
	putStrLn =<< gdkDeviceGetName kbd
	print =<< gdkDisplayDeviceIsGrabbed d kbd
	gdkDisplayGetPrimaryMonitor d >>= \case
		Nothing -> putStrLn "no primary monitor"
		Just mntr -> do
			print mntr
			print =<< gdkMonitorGetManufacturer mntr
			print =<< gdkMonitorGetModel mntr
	putStrLn . ("Supports cursor color: " ++) . show =<< gdkDisplaySupportsCursorColor d
	putStrLn . ("Supports cursor alpha: " ++) . show =<< gdkDisplaySupportsCursorAlpha d
	putStrLn . ("Default cursor size: " ++) . show =<< gdkDisplayGetDefaultCursorSize d
	putStrLn . ("Maximal cursor size: " ++) . show =<< gdkDisplayGetMaximalCursorSize d
	putStrLn . ("Number of monitors: " ++) . show =<< gdkDisplayGetNMonitors d
	scrn <- gdkScreenGetDefault
	vsl <- gdkScreenGetSystemVisual scrn
	putStrLn . ("Depth of system visual: " ++) . show =<< gdkVisualGetDepth vsl
	maybe (putStrLn "No rgba visual") (\v -> putStrLn . ("Depth of rgba visual: " ++) . show =<< gdkVisualGetDepth v)
		=<< gdkScreenGetRgbaVisual scrn
	putStrLn . ("Screen is composited: " ++) . show =<< gdkScreenIsComposited scrn
	rtwn <- gdkScreenGetRootWindow scrn
	putStrLn . ("Window type of root window: " ++) . show =<< gdkWindowGetWindowType rtwn
	putStrLn . ("State of root window: " ++) . show =<< gdkWindowGetState rtwn
	gdkScreenListVisuals scrn >>= \case
		([], []) -> putStrLn "no visuals"
		(_, []) -> putStrLn "no post visuals"
		([], vs) -> do
			putStrLn "no pre visuals"
			ds <- for vs gdkVisualGetDepth
			putStrLn $ "Depth of visuals: " ++ show ((head &&& length) <$> group ds)
			ts <- for vs gdkVisualGetVisualType
			putStrLn $ "Types of visuals: " ++ show ((head &&& length) <$> group ts)
			for_ vs \v -> do
				putStrLn . ("Red pixel details of visual: " ++) . show =<< gdkVisualGetRedPixelDetails v
				putStrLn . ("Green pixel details of visual: " ++) . show =<< gdkVisualGetGreenPixelDetails v
				putStrLn . ("Blue pixel details of visual: " ++) . show =<< gdkVisualGetBluePixelDetails v
		(_, _) -> putStrLn "pre and post visuals"
	let wattr = mkGdkWindowAttr [
				gdkExposureMask, gdkButtonPressMask, gdkKeyPressMask, gdkFocusChangeMask,
				gdkEnterNotifyMask, gdkLeaveNotifyMask, gdkPointerMotionMask,
				gdkAllEventsMask
				]
			400 400
			gdkInputOutput gdkWindowToplevel
	w <- gdkWindowNew Nothing wattr { gdkWindowAttrTitle = Just "試験窓" }
	print =<< gdkWindowGetWindowType w
	print gdkWindowToplevel
	putStrLn =<< gdkDisplayGetName =<< gdkWindowGetDisplay w
	print =<< gdkScreenGetResolution =<< gdkWindowGetScreen w
	print =<< gdkVisualGetDepth =<< gdkWindowGetVisual w
	putStrLn . ("Window is destroyed: " ++) . show =<< gdkWindowIsDestroyed w
	putStrLn . ("Window is visible: " ++) . show =<< gdkWindowIsVisible w
	putStrLn . ("Window is viewable: " ++) . show =<< gdkWindowIsViewable w
	putStrLn . ("Window is input only: " ++) . show =<< gdkWindowIsInputOnly w
	putStrLn . ("Window is shaped: " ++) . show =<< gdkWindowIsShaped w
	putStrLn . ("Window state: " ++) . show =<< gdkWindowGetState w
	gdkWindowShow w
	gdkWindowSetOpacity w 0.5
	putStrLn . ("Window is visible: " ++) . show =<< gdkWindowIsVisible w
	putStrLn . ("Window is viewable: " ++) . show =<< gdkWindowIsViewable w
	putStrLn . ("Window state: " ++) . show =<< gdkWindowGetState w
	gdkWindowSetEvents w [gdkExposureMask, gdkButtonPressMask, gdkFocusChangeMask, gdkKeyPressMask]
	print gdkExposureMask
	gdkWindowInvalidateRect w (50, 50) (100, 100) False
	gdkWindowFreezeUpdates w
	gdkWindowThawUpdates w
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkEventGet >>= \case
			Just e -> do
				b <- checkEvent e
				pure if b then Nothing else Just False
			Nothing -> pure $ Just True
	gdkWindowDestroy w
	putStrLn . ("Window is destroyed: " ++) . show =<< gdkWindowIsDestroyed w
	putStrLn . ("Window is visible: " ++) . show =<< gdkWindowIsVisible w
	putStrLn . ("Window is viewable: " ++) . show =<< gdkWindowIsViewable w

checkEvent :: GdkEvent -> IO Bool
checkEvent = \case
	GdkEventGdkNothing n -> do
		putStrLn $ "GDK_NOTHING: " ++ show n
		pure True
	GdkEventGdkDelete d -> do
		putStrLn $ "GDK_DELETE: " ++ show d
		pure False
	GdkEventGdkKeyPress k -> do
		w <- gdkEventKeyWindow k
		kv <- gdkEventKeyKeyval k
		putStrLn $ "GDK_KEY_PRESS: " ++ show k ++ ": " ++ show kv
		when (kv == fromIntegral (ord 'h')) $ do
			putStrLn "`h' pressed"
			gdkWindowHide w
--			threadDelay 1000000
			threadDelay 300000
			gdkWindowShow w
--			void getChar
		when (kv == fromIntegral (ord 'i')) $ do
			putStrLn "`i' pressed"
			gdkWindowIconify w
		when (kv == fromIntegral (ord 'm')) $ do
			putStrLn "`m' pressed"
			gdkWindowMaximize w
		when (kv == fromIntegral (ord 'f')) $ do
			putStrLn "`f' pressed"
			gdkWindowFullscreen w
		pure $ kv /= fromIntegral (ord 'q')
	GdkEventGdkKeyRelease k -> do
		kv <- gdkEventKeyKeyval k
		putStrLn $ "GDK_KEY_RELEASE: " ++ show k ++ ": " ++ show kv
		pure True
	GdkEventGdkFocusChange f -> do
		i <- gdkEventFocusIn f
		putStrLn $ "GDK_FOCUS_CHANGE: " ++ show f ++ ": " ++ show i
		pure True
	GdkEventGdkMap m -> do
		putStrLn $ "GDK_MAP: " ++ show m
		pure True
	GdkEventGdkUnmap m -> do
		putStrLn $ "GDK_UNMAP: " ++ show m
		pure True
	GdkEventGdkConfigure c -> do
		w <- gdkEventConfigureWidth c
		h <- gdkEventConfigureHeight c
		putStrLn $ "GDK_CONFIGURE: " ++ show c ++ ": " ++ show w ++ " " ++ show h
		pure True
	GdkEventGdkVisibilityNotify v -> do
		w <- gdkEventVisibilityWindow v
		vs <- gdkEventVisibilityState v
		r <- cairoRegionCreateRectangle $ CairoRectangleIntT 50 50 100 100
		do
			gdkWindowWithDrawFrame w r \cxt -> do
				cr <- gdkDrawingContextGetCairoContext cxt
				cairoSetSourceRgb cr 0.8 0.2 0.2
				cairoSetLineWidth cr 5
				cairoMoveTo cr 10 10
				cairoLineTo cr 90 90
				cairoStroke cr
		putStrLn $ "GDK_VISIBILITY_NOTIFY: " ++ show v ++ ": " ++ show vs
		pure True
	GdkEventGdkWindowState s -> do
		ns <- gdkEventWindowStateNewWindowState s
		putStrLn $ "GDK_WINDOW_STATE: " ++ show s ++ ": " ++ show ns
		pure True
	e -> do	print e
		pure True

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = bool (pure ()) (doWhile_ act) =<< act

doWhile :: Monad m => m (Maybe a) -> m a
doWhile act = maybe (doWhile act) pure =<< act
