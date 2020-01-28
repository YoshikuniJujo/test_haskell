{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

#include <X11/Xlib.h>

module Field (
	Field, openField, isDeleteEvent, destroyField, closeField,
	Mask, exposureMask, keyPressMask,
		buttonPressMask, buttonReleaseMask, pointerMotionMask,
	Event(..), withNextEvent,
	fillRect, clearField, flushField ) where

import Data.Bits
import Graphics.X11
import Graphics.X11.Xlib.Extras

import TextLike

data Field = Field {
	display :: Display,
	window :: Window,
	pixmap :: Pixmap,
	graphicsContext :: GC,
	isDeleteEvent :: Event -> Bool }

openField :: TextUtf8 t => t -> [Mask] -> IO Field
openField nm ms = do
	_ <- initThreads
	dpy <- openDisplay ""
	wmp <- internAtom dpy "WM_PROTOCOLS" True
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	wmn <- internAtom dpy "_NET_WM_NAME" True
	ut8 <- internAtom dpy "UTF8_STRING" True
	let	scr = defaultScreen dpy
		(blk, wht) = (blackPixel dpy scr, whitePixel dpy scr)
	rt <- rootWindow dpy scr

	win <- createSimpleWindow dpy rt 0 0 100 100 1 blk wht
	pxm <- createPixmap dpy win 1000 1000 $ defaultDepth dpy scr
	selectInput dpy win $ foldr (.|.) structureNotifyMask ms
	setWMProtocols dpy win [del]
	changeProperty8 dpy win wmn ut8 #{const PropModeReplace} $ utf8 nm
	mapWindow dpy win

	gc <- createGC dpy rt

	fillRectangle dpy pxm gc 0 0 1000 1000

	pure . Field dpy win pxm gc $ \case
		ClientMessageEvent { ev_message_type = t, ev_data = d0 : _ } ->
			t == wmp && d0 == fromIntegral del
		_ -> False

destroyField :: Field -> IO ()
destroyField Field { display = d, window = w } = destroyWindow d w

closeField :: Field -> IO ()
closeField Field { display = d } = do
	setCloseDownMode d #const AllTemporary
	closeDisplay d

withNextEvent :: Field -> (Event -> IO a) -> IO a
withNextEvent Field { display = d } act =
	allocaXEvent $ \e -> act =<< nextEvent d e *> getEvent e

fillRect :: Field ->
	Pixel -> Position -> Position -> Dimension -> Dimension -> IO ()
fillRect Field { display = dpy, pixmap = win, graphicsContext = gc } c x y w h =
	setForeground dpy gc c >> fillRectangle dpy win gc x y w h

clearField :: Field -> IO ()
clearField Field { display = dpy, pixmap = win, graphicsContext = gc } =
	setForeground dpy gc 0x000000 >> fillRectangle dpy win gc 0 0 1000 1000 -- clearWindow dpy win

flushField :: Field -> IO ()
flushField Field { display = dpy, window = win, pixmap = pxm, graphicsContext = gc } = do
	copyArea dpy pxm win gc 0 0 1000 1000 0 0
	flush dpy
