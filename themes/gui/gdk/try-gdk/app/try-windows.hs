{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad
import Control.Concurrent
import Data.Foldable
import Data.Maybe
import Data.Color
import Data.IORef
import Data.KeySym
import Text.Read
import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.PointsAndRectangles
import Graphics.Gdk.Cursors
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.GdkDrawingContext

import Graphics.Cairo.Drawing.CairoT

import Try.Tools

main :: IO ()
main = do
	skipTaskbar <- newIORef False
	skipPager <- newIORef False
	urgency <- newIORef False
	let	opts = [
			optHelp, optWindowShowAndHide, optDisplayScreen, optShowDevice,
			optShowEvents, optMainLoop,
			optWindowEvents, optNoEventCompression, optTitle,
			optCursor, optWindowInfo, optRaise, optFocus, optTypeHint,
			optDecoration ]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	putStrLn `mapM_` es
	when (OptHelp `elem` ss) . putStr $ usageInfo "try-windows" opts
	dpy <- gdkDisplayOpen ""
	let	scr = gdkDisplayGetDefaultScreen dpy
	wr <- gdkScreenGetRootWindow scr
	w0 <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits []) 900 700
	w1 <- gdkToplevelNew (Just wr) $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits []) 450 350
	wc <- gdkToplevelNew (Just w0) $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits []) 500 300

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

	runOpt dpy w0 ss

	when (OptShowDevice `elem` ss) do
		putStrLn =<< gdkDeviceGetName pnt
		print =<< gdkDeviceGetSource pnt
		for_ pnts \p -> do
			putStrLn =<< gdkDeviceGetName p
			print =<< gdkDeviceGetSource p
		putStrLn ""
		putStrLn =<< gdkDeviceGetName kbd
		(putStrLn <=< gdkDeviceGetName) `mapM_` kbds

	gdkWindowSetEvents w0 . gdkEventMaskMultiBits $ getOptWindowEventMask ss
	when (OptShowEvents `elem` ss) do
		print . gdkEventMaskSingleBitList =<< gdkWindowGetEvents w0

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
			GdkEventGdkFocusChange f -> True <$ do
				print =<< gdkEventFocus f
				r <- gdkWindowGetVisibleRegion w0
				gdkWindowWithDrawFrame w0 r \dc -> do
					cr <- gdkDrawingContextGetCairoContext dc
					cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0.5 0
					cairoPaint cr
			GdkEventGdkWindowState e -> True <$ do
				s <- gdkEventWindowState e
				print . gdkWindowStateList
					$ gdkEventWindowStateChangedMask s
				print . gdkWindowStateList
					$ gdkEventWindowStateNewWindowState s
				print . gdkWindowStateList=<< gdkWindowGetState w0
			GdkEventGdkConfigure e -> True <$ do
				print =<< gdkEventConfigure e
				print =<< gdkWindowGetGeometry w0
				print =<< getPositionAndSize w0
				print =<< gdkWindowGetOrigin w0
				print =<< gdkWindowGetRootCoords w0 100 200
				print =<< getFrameExtents w0
				print =<< gdkWindowGetRootOrigin w0
			GdkEventGdkKeyPress e -> do
				k <- gdkEventKey e
				print k
				let	ts = gdkEventKeyTime k
				case gdkEventKeyKeyval k of
					Xk_q -> pure False
					Xk_e -> True <$ do
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
					Xk_w -> True <$ do
						gdkWindowWithdraw w0
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
						gdkDisplayFlush dpy
						threadDelay 1000000
						gdkWindowShow w0
					Xk_i -> True <$ do
						gdkWindowIconify w0
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
						gdkDisplayFlush dpy
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
						threadDelay 1000000
						gdkWindowDeiconify w0
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
					Xk_t -> True <$ do
						gdkWindowStick w0
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
					Xk_u -> True <$ do
						gdkWindowUnstick w0
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
					Xk_m -> True <$ do
						gdkWindowMaximize w0
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
					Xk_n -> True <$ do
						gdkWindowUnmaximize w0
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
					Xk_f -> True <$ do
						gdkWindowFullscreen w0
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
					Xk_g -> True <$ do
						gdkWindowUnfullscreen w0
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
					Xk_h -> True <$
						fullScreenModeAllMonitor w0
					Xk_j -> True <$
						fullScreenModeCurrent w0
					Xk_a -> True <$ do
						gdkWindowSetKeepAbove w0 True
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
					Xk_b -> True <$ do
						gdkWindowSetKeepBelow w0 True
						print . gdkWindowStateList
							=<< gdkWindowGetState w0
					Xk_l -> True <$ do
						gdkWindowLower w0
						gdkDisplayFlush dpy
						forkIO do
							threadDelay 1000000
							when (OptRaise `elem` ss) $ gdkWindowRaise w0
							when (OptFocus `elem` ss) $ gdkWindowFocus w0 ts
							print ts
							print . gdkWindowStateList =<< gdkWindowGetState w0
					Xk_o -> True <$ do
						gdkWindowSetOpacity w0 0.5
					Xk_p -> True <$ do
						gdkWindowSetOpacity w0 1
					Xk_r -> True <$ do
						gdkWindowSetTransientFor w1 w0
						gdkWindowShow w1
					Xk_d -> True <$ do
						b <- gdkWindowGetModalHint w1
						gdkWindowSetModalHint w1 $ not b
						print =<< gdkWindowGetModalHint w1
					Xk_y -> True <$ do
						b <- not <$> readIORef skipTaskbar
						gdkWindowSetSkipTaskbarHint w0 b
						writeIORef skipTaskbar b
					Xk_v -> True <$ do
						b <- not <$> readIORef skipPager
						gdkWindowSetSkipPagerHint w0 b
						writeIORef skipPager b
					Xk_x -> True <$ do
						threadDelay 1000000
						b <- not <$> readIORef urgency
						gdkWindowSetUrgencyHint w0 b
						writeIORef urgency b
						print b
					_ -> pure True
			GdkEventGdkAny e -> True <$ (print =<< gdkEventAny e)

	when (OptWindowInfo `elem` ss) do
		print =<< gdkWindowIsDestroyed w0
		print =<< gdkWindowIsVisible w0
		print =<< gdkWindowIsViewable w0
		print =<< gdkWindowIsInputOnly w0
		print =<< gdkWindowIsShaped w0

	gdkWindowDestroy w0

	when (OptWindowInfo `elem` ss) do
		print =<< gdkWindowIsDestroyed w0

fullScreenModeAllMonitor :: GdkWindow -> IO ()
fullScreenModeAllMonitor w = do
	gdkWindowSetFullscreenMode w GdkFullscreenOnAllMonitors
	print =<< gdkWindowGetFullscreenMode w

fullScreenModeCurrent :: GdkWindow -> IO ()
fullScreenModeCurrent w = do
	gdkWindowSetFullscreenMode w GdkFullscreenOnCurrentMonitor
	print =<< gdkWindowGetFullscreenMode w


data OptSetting
	= OptHelp
	| OptWindowShowAndHide
	| OptDisplayScreen
	| OptShowDevice
	| OptShowEvents
	| OptMainLoop
	| OptWindowEvents [GdkEventMaskSingleBit]
	| OptNoEventCompression
	| OptTitle String
	| OptCursor GdkCursorType
	| OptWindowInfo
	| OptRaise
	| OptFocus
	| OptTypeHint GdkWindowTypeHint
	| OptDecoration [GdkWmDecoration]
	deriving (Show, Eq)

optHelp, optWindowShowAndHide, optDisplayScreen, optShowDevice,
	optShowEvents, optMainLoop, optWindowEvents, optNoEventCompression,
	optTitle, optCursor, optWindowInfo, optRaise, optFocus, optTypeHint,
	optDecoration ::
	OptDescr OptSetting
optHelp = Option ['h'] ["help"] (NoArg OptHelp) "Show help"

optWindowShowAndHide = Option ['s'] ["show-and-hide"]
	(NoArg OptWindowShowAndHide) "Show and Hide some windows"

optDisplayScreen = Option ['d'] ["display-screen-etc"]
	(NoArg OptDisplayScreen) "Get display, screen and so on"

optShowDevice = Option ['v'] ["show-device"] (NoArg OptShowDevice) "Show device"

optShowEvents = Option ['e'] ["show-events"] (NoArg OptShowEvents) "Show events"

optMainLoop = Option ['l'] ["main-loop"] (NoArg OptMainLoop) "Go to main loop"

optWindowEvents = Option ['w'] ["window-events"]
	(ReqArg (OptWindowEvents . readEventMask) "Event Mask")
	"Set Window Event Mask"

optNoEventCompression = Option ['c'] ["no-event-compression"]
	(NoArg OptNoEventCompression) "Disable event compression"

optTitle = Option ['t'] ["title"] (ReqArg OptTitle "Title") "Set title"

optCursor = Option [] ["cursor"] (ReqArg (OptCursor . read) "Cursor Type")
	"Set cursor"

optWindowInfo = Option ['i'] ["info"] (NoArg OptWindowInfo)
	"Show window information"

optRaise = Option ['r'] ["raise"] (NoArg OptRaise) "Do raise"

optFocus = Option ['f'] ["focus"] (NoArg OptFocus) "Do focus"

optTypeHint = Option [] ["type-hint"] (ReqArg (OptTypeHint . read) "TypeHint")
	"Set window type hint"

optDecoration = Option [] ["decoration"] (ReqArg (OptDecoration . read) "Decoration")
	"Set decoration"

readEventMask :: String -> [GdkEventMaskSingleBit]
readEventMask = fromMaybe [] . readMaybe

getOptWindowEventMask :: [OptSetting] -> [GdkEventMaskSingleBit]
getOptWindowEventMask [] = []
getOptWindowEventMask (OptWindowEvents ems : _) = ems
getOptWindowEventMask (_ : ss) = getOptWindowEventMask ss
	
runOpt :: GdkDisplay -> GdkWindow -> [OptSetting] -> IO ()
runOpt _ _ [] = pure ()
runOpt d w (OptNoEventCompression : ss) = do
	print =<< gdkWindowGetEventCompression w
	gdkWindowSetEventCompression w False
	print =<< gdkWindowGetEventCompression w
	runOpt d w ss
runOpt d w (OptTitle t : ss) = gdkWindowSetTitle w t >> runOpt d w ss
runOpt d w (OptCursor ct : ss) = do
	c <- gdkCursorNewForDisplay d ct
	gdkWindowSetCursor w c
	Just c' <- gdkWindowGetCursor w
	print =<< gdkCursorGetCursorType c'
	runOpt d w ss
runOpt d w (OptWindowInfo : ss) = do
	print =<< gdkWindowGetWindowType w
	print =<< gdkWindowGetFullscreenMode w
	print =<< gdkWindowGetGeometry w
	print =<< getPositionAndSize w
	print =<< gdkWindowGetOrigin w
	print =<< gdkWindowGetRootCoords w 100 200
	print =<< getFrameExtents w
	print =<< gdkWindowGetRootOrigin w
	print =<< gdkWindowGetTypeHint w
	print . (gdkWmDecorationList <$>) =<< gdkWindowGetDecorations w
	runOpt d w ss
runOpt d w (OptTypeHint t : ss) =
	gdkWindowSetTypeHint w t >> runOpt d w ss
runOpt d w (OptDecoration dc : ss) = do
	putStr "Decoration: " >> print dc
	gdkWindowSetDecorations w $ gdkWmDecorations dc
	runOpt d w ss
runOpt d w (_ : ss) = runOpt d w ss

getPositionAndSize :: GdkWindow -> IO ((CInt, CInt), (CInt, CInt))
getPositionAndSize w = (,)
	<$> gdkWindowGetPosition w
	<*> ((,) <$> gdkWindowGetWidth w <*> gdkWindowGetHeight w)

getFrameExtents :: GdkWindow -> IO GdkRectangle
getFrameExtents w = do
	r <- gdkRectangleNew
	gdkWindowGetFrameExtents w r
	gdkRectangleFreeze r
