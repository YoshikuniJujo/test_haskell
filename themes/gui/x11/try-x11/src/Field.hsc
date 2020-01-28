{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

#include <X11/Xlib.h>

module Field (
	Field, openField, isDeleteEvent, destroyField, closeField,
	Event(..), withNextEvent ) where

import Graphics.X11
import Graphics.X11.Xlib.Extras

data Field = Field {
	display :: Display,
	window :: Window,
	isDeleteEvent :: Event -> Bool }

openField :: IO Field
openField = do
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
	pure . Field dpy wmp $ \case
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
