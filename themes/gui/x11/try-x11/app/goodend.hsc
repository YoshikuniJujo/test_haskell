{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

#include <X11/Xlib.h>

module Main where

import Graphics.X11 (
	initThreads,
	Display, openDisplay, closeDisplay, rootWindow, setCloseDownMode,
	{- ScreenNumber, -} defaultScreen, blackPixel, whitePixel,
	Window, createSimpleWindow, mapWindow, destroyWindow,
		selectInput, setWMProtocols, structureNotifyMask,
	Atom, internAtom,
	{- XEvent, -} allocaXEvent, nextEvent
	)
import Graphics.X11.Xlib.Extras (Event(..), getEvent)

openWindow :: IO (Display, Window, Atom, Atom)
openWindow = do
	_ <- initThreads
	dpy <- openDisplay ""
	wmp <- internAtom dpy "WM_PROTOCOLS" True
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let	scr = defaultScreen dpy
		(blk, wht) = (blackPixel dpy scr, whitePixel dpy scr)
	rt <- rootWindow dpy scr
	win <- createSimpleWindow dpy rt 0 0 100 100 1 blk wht
	selectInput dpy win structureNotifyMask
	setWMProtocols dpy win [del]
	mapWindow dpy win
	return (dpy, win, wmp, del)

loop :: Display -> Window -> Atom -> Atom -> IO ()
loop dpy win wmp del = allocaXEvent $ \e -> do
	ev <- nextEvent dpy e *> getEvent e
	case ev of
		ClientMessageEvent { ev_message_type = t, ev_data = d }
			| t == wmp && d !! 0 == fromIntegral del ->
				destroyWindow dpy win >> loop dpy win wmp del
		DestroyWindowEvent {} -> do
			setCloseDownMode dpy #const AllTemporary
			closeDisplay dpy
		_ -> loop dpy win wmp del

main :: IO ()
main = uncurry4 loop =<< openWindow where uncurry4 f (x, y, z, w) = f x y z w
