{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Bits
import Data.Char
import Codec.Binary.UTF8.String
import Graphics.X11
import Graphics.X11.Xlib.Extras

openWindow :: IO (Display, Window, GC, Atom, Atom)
openWindow = do
	_ <- initThreads
	dpy <- openDisplay ""
	wm <- internAtom dpy "WM_PROTOCOLS" True
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let	scr = defaultScreen dpy
	root <- rootWindow dpy scr
	win <- createSimpleWindow dpy root 0 0 100 100 1
		(blackPixel dpy scr) (whitePixel dpy scr)
	gc <- createGC dpy win
	setBackground dpy gc 0x000000
	storeName dpy win -- $ encodeString
		"try-x11-exe"
		-- "X11を試してみる"
	wmn <- internAtom dpy "_NET_WM_NAME" True
	u8 <- internAtom dpy "UTF8_STRING" True
--	changeProperty8 dpy win wmn u8 0 {- PropModeReplace -} (fromIntegral . ord <$> "hello")
	changeProperty8 dpy win wmn u8 0 {- PropModeReplace -} (fromIntegral . ord <$> encodeString "あいうえお")
	setWMProtocols dpy win [del]
	selectInput dpy win $
		structureNotifyMask .|. exposureMask .|. keyPressMask .|.
		buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask
	mapWindow dpy win
	setWindowBackground dpy win 0x000000
	clearWindow dpy win
	return (dpy, win, gc, wm, del)

loop :: Display -> Window -> GC -> Atom -> Atom -> IO ()
loop dpy win gc wm del = allocaXEvent $ \e -> do
	nextEvent dpy e
	ev <- getEvent e
	case ev of
		KeyEvent {} -> do
			ch <- fmap (chr . fromEnum) $ keycodeToKeysym dpy
				(ev_keycode ev) 0
			when (ch == 'q') $ destroyWindow dpy win
			loop dpy win gc wm del
		ExposeEvent {} -> do
			setForeground dpy gc 0xffffff
--			drawRectangle dpy win gc 150 100 300 200
			fillRectangle dpy win gc 150 100 300 200
			setForeground dpy gc 0xff0000
			fillRectangle dpy win gc 200 150 300 200
			setForeground dpy gc 0x00ff00
			fillRectangle dpy win gc 250 200 300 200
			setForeground dpy gc 0x0000ff
			fillRectangle dpy win gc 300 250 300 200
			loop dpy win gc wm del
		ClientMessageEvent {}
			| ev_message_type ev == wm && ev_data ev !! 0 == fromIntegral del -> do
				putStrLn "close window"
				destroyWindow dpy win
				loop dpy win gc wm del
		DestroyWindowEvent {} -> do
			print ev
			setCloseDownMode dpy 0 -- AllTemporary
			closeDisplay dpy
		_ -> print ev >> loop dpy win gc wm del

main :: IO ()
main = do
	(dpy, win, gc, wm, del) <- openWindow
	print wm
	print del
	flush dpy
	loop dpy win gc wm del
