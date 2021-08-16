{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Text.Read
import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask

main :: IO ()
main = do
	let	opts = [
			optHelp, optWindowShowAndHide, optDisplayScreen, optShowDevice,
			optWindowEvents ]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	putStrLn `mapM_` es
	when (OptHelp `elem` ss) . putStr $ usageInfo "try-windows" opts
	dpy <- gdkDisplayOpen ""
	let	scr = gdkDisplayGetDefaultScreen dpy
	wr <- gdkScreenGetRootWindow scr
	w0 <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [GdkPointerMotionMask])
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

	when (OptShowDevice `elem` ss) do
		print pnt
		print kbd

	print . gdkEventMaskSingleBitList =<< gdkWindowGetEvents w0
	print . gdkEventMaskSingleBitList =<< gdkWindowGetDeviceEvents w0 pnt
	gdkWindowSetEvents w0 . gdkEventMaskMultiBits $ getOptWindowEventMask ss
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

data OptSetting
	= OptHelp
	| OptWindowShowAndHide
	| OptDisplayScreen
	| OptShowDevice
	| OptWindowEvents [GdkEventMaskSingleBit]
	deriving (Show, Eq)

optHelp, optWindowShowAndHide, optDisplayScreen, optShowDevice,
	optWindowEvents ::
	OptDescr OptSetting
optHelp = Option ['h'] ["help"] (NoArg OptHelp) "Show help"

optWindowShowAndHide = Option ['s'] ["show-and-hide"]
	(NoArg OptWindowShowAndHide) "Show and Hide some windows"

optDisplayScreen = Option ['d'] ["display-screen-etc"]
	(NoArg OptDisplayScreen) "Get display, screen and so on"

optShowDevice = Option ['v'] ["show-device"] (NoArg OptShowDevice) "Show Device"

optWindowEvents = Option ['w'] ["window-events"]
	(ReqArg (OptWindowEvents . readEventMask) "Event Mask")
	"Set Window Event Mask"

readEventMask :: String -> [GdkEventMaskSingleBit]
readEventMask = fromMaybe [] . readMaybe

getOptWindowEventMask :: [OptSetting] -> [GdkEventMaskSingleBit]
getOptWindowEventMask [] = []
getOptWindowEventMask (OptWindowEvents ems : _) = ems
getOptWindowEventMask (_ : ss) = getOptWindowEventMask ss
