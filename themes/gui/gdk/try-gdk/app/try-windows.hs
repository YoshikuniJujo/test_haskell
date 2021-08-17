{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Map.Strict ((!?))
import Data.Maybe
import Text.Read
import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Try.Tools

import qualified Data.Map.Strict as M

main :: IO ()
main = do
	let	opts = [
			optHelp, optWindowShowAndHide, optDisplayScreen, optShowDevice,
			optShowEvents, optMainLoop,
			optEvents, optWindowEvents, optPointerDeviceEvents,
			optPointerPhysicalDevice ]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	putStrLn `mapM_` es
	when (OptHelp `elem` ss) . putStr $ usageInfo "try-windows" opts
	dpy <- gdkDisplayOpen ""
	let	scr = gdkDisplayGetDefaultScreen dpy
	wr <- gdkScreenGetRootWindow scr
	w0 <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [])
		900 700 GdkInputOutput GdkWindowToplevel
	w1 <- gdkWindowNew (Just wr) $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [])
		900 700 GdkInputOutput GdkWindowToplevel
	wc <- gdkWindowNew (Just w0) $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [])
		500 300 GdkInputOutput GdkWindowChild

	when (OptDisplayScreen `elem` ss) do
		print dpy
		print $ gdkWindowGetDisplay w0
		print scr
		print $ gdkWindowGetScreen w0
		print =<< gdkScreenGetSystemVisual scr
		print =<< gdkScreenGetRgbaVisual scr
		print $ gdkWindowGetVisual w0
		print =<< gdkScreenGetRootWindow scr
		print =<< gdkGetDefaultRootWindow
		print w0
		print =<< gdkWindowGetToplevel w0
		print wr
		print =<< gdkWindowGetParent w0
		print w0
		print =<< gdkWindowGetParent wc
		gdkWindowReparent wc w1 100 100
		print w1
		print =<< gdkWindowGetParent wc
		print wc
		print =<< gdkWindowPeekChildren w1

	st <- gdkDisplayGetDefaultSeat dpy
	pnt <- gdkSeatGetPointer st
	kbd <- gdkSeatGetKeyboard st
	pnts <- gdkDeviceListSlaveDevices pnt
	kbds <- gdkDeviceListSlaveDevices kbd

	pntt <- getPhysicalDeviceTable pnt

	runOpt w0 pntt ss

	when (OptShowDevice `elem` ss) do
		putStrLn =<< gdkDeviceGetName pnt
		(putStrLn <=< gdkDeviceGetName) `mapM_` pnts
		putStrLn ""
		putStrLn =<< gdkDeviceGetName kbd
		(putStrLn <=< gdkDeviceGetName) `mapM_` kbds

	gdkWindowSetEvents w0 . gdkEventMaskMultiBits $ getOptWindowEventMask ss
	gdkWindowSetDeviceEvents w0 pnt . gdkEventMaskMultiBits
		$ getOptPointerDeviceEventMask ss
	when (OptShowEvents `elem` ss) do
		print . gdkEventMaskSingleBitList =<< gdkWindowGetEvents w0
		print . gdkEventMaskSingleBitList =<< gdkWindowGetDeviceEvents w0 pnt

	when (OptWindowShowAndHide `elem` ss) do
		gdkWindowShow w0
		gdkWindowShow wc
		gdkDisplayFlush dpy
		threadDelay 1000000
		gdkWindowShow w1
		gdkDisplayFlush dpy
		threadDelay 1000000
		gdkWindowHide w0
		gdkDisplayFlush dpy
		threadDelay 1000000
		gdkWindowLower w0
		gdkWindowShowUnraised w0
		gdkDisplayFlush dpy
		threadDelay 1000000
		gdkWindowShow w0
		gdkDisplayFlush dpy
		threadDelay 1000000
		gdkWindowDestroy w0
		gdkDisplayFlush dpy
		threadDelay 1000000

	when (OptMainLoop `elem` ss) do
		gdkWindowShow w0
		mainLoop \case
			GdkEventGdkDelete _d -> pure False
			GdkEventGdkKeyPress
				(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q)
					-> pure False
			GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

data OptSetting
	= OptHelp
	| OptWindowShowAndHide
	| OptDisplayScreen
	| OptShowDevice
	| OptShowEvents
	| OptMainLoop
	| OptEvents [GdkEventMaskSingleBit]
	| OptWindowEvents [GdkEventMaskSingleBit]
	| OptPointerDeviceEvents [GdkEventMaskSingleBit]
	| OptPointerPhysicalDevice String
	deriving (Show, Eq)

optHelp, optWindowShowAndHide, optDisplayScreen, optShowDevice,
	optShowEvents, optMainLoop,
	optEvents, optWindowEvents, optPointerDeviceEvents, optPointerPhysicalDevice ::
	OptDescr OptSetting
optHelp = Option ['h'] ["help"] (NoArg OptHelp) "Show help"

optWindowShowAndHide = Option ['s'] ["show-and-hide"]
	(NoArg OptWindowShowAndHide) "Show and Hide some windows"

optDisplayScreen = Option ['d'] ["display-screen-etc"]
	(NoArg OptDisplayScreen) "Get display, screen and so on"

optShowDevice = Option ['v'] ["show-device"] (NoArg OptShowDevice) "Show device"

optShowEvents = Option ['e'] ["show-events"] (NoArg OptShowEvents) "Show events"

optMainLoop = Option ['l'] ["main-loop"] (NoArg OptMainLoop) "Go to main loop"

optEvents = Option [] ["events"]
	(ReqArg (OptEvents . readEventMask) "Event Mask")
	"Set event mask"

optWindowEvents = Option ['w'] ["window-events"]
	(ReqArg (OptWindowEvents . readEventMask) "Event Mask")
	"Set Window Event Mask"

optPointerDeviceEvents = Option ['p'] ["pointer-device-events"]
	(ReqArg (OptPointerDeviceEvents . readEventMask) "Event Mask")
	"Set pointer device event mask"

optPointerPhysicalDevice = Option [] ["pointer-physical-device"]
	(ReqArg OptPointerPhysicalDevice "Device Name")
	"Set pointer physical device name"

readEventMask :: String -> [GdkEventMaskSingleBit]
readEventMask = fromMaybe [] . readMaybe

getOptWindowEventMask, getOptPointerDeviceEventMask :: [OptSetting] -> [GdkEventMaskSingleBit]
getOptWindowEventMask [] = []
getOptWindowEventMask (OptWindowEvents ems : _) = ems
getOptWindowEventMask (_ : ss) = getOptWindowEventMask ss

getOptPointerDeviceEventMask [] = []
getOptPointerDeviceEventMask (OptPointerDeviceEvents ems : _) = ems
getOptPointerDeviceEventMask (_ : ss) = getOptPointerDeviceEventMask ss

getPhysicalDeviceTable :: GdkDeviceMaster pk -> IO (M.Map String (GdkDevicePhysical pk))
getPhysicalDeviceTable m = (M.fromList <$>)
	$ gdkDeviceListSlaveDevices m >>=
		mapM \s -> (,) <$> gdkDeviceGetName s <*> pure s
	

runOpt :: GdkWindow -> M.Map String (GdkDevicePhysical 'Pointer) -> [OptSetting] -> IO ()
runOpt _ _ [] = pure ()
runOpt w ppt (OptPointerPhysicalDevice ppn : OptEvents ems : ss) = do
	case ppt !? ppn of
		Nothing -> putStrLn $ "No such name device: " ++ ppn
		Just pp -> do
			gdkWindowSetDeviceEvents w pp $ gdkEventMaskMultiBits ems
			print . gdkEventMaskSingleBitList =<< gdkWindowGetDeviceEvents w pp
	runOpt w ppt ss
runOpt w ppt (_ : ss) = runOpt w ppt ss
