{-# LANGUAGE LambdaCase, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

#include <X11/Xlib.h>

module Field (
	Field, openField, isDeleteEvent, destroyField, closeField,
	Mask, exposureMask, keyPressMask,
		buttonPressMask, buttonReleaseMask,
		pointerMotionMask, button1MotionMask,
	Event(..), withNextEvent, withNextEventTimeout,
	Position, Dimension, Pixel, fillRect, clearField, flushField ) where

import Control.Monad.Trans.Control
import Data.Bits
import Data.Word
import Graphics.X11
import Graphics.X11.Xlib.Extras

import TextLike

data Field = Field {
	display :: Display,
	window :: Window,
	pixmap :: Pixmap,
	screenWidth :: Dimension,
	screenHeight :: Dimension,
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
		scrW = fromIntegral $ displayWidth dpy scr
		scrH = fromIntegral $ displayHeight dpy scr
	rt <- rootWindow dpy scr

	win <- createSimpleWindow dpy rt 0 0 100 100 1 blk wht
	pxm <- createPixmap dpy win scrW scrH $ defaultDepth dpy scr
	selectInput dpy win $ foldr (.|.) structureNotifyMask ms
	setWMProtocols dpy win [del]
	changeProperty8 dpy win wmn ut8 #{const PropModeReplace} $ utf8 nm
	mapWindow dpy win

	gc <- createGC dpy rt
	setGraphicsExposures dpy gc False

	fillRectangle dpy pxm gc 0 0 scrW scrH

	pure . Field dpy win pxm scrW scrH gc $ \case
		ClientMessageEvent { ev_message_type = t, ev_data = d0 : _ } ->
			t == wmp && d0 == fromIntegral del
		_ -> False

destroyField :: Field -> IO ()
destroyField Field { display = d, window = w } = destroyWindow d w

closeField :: Field -> IO ()
closeField Field { display = d } = do
	setCloseDownMode d #const AllTemporary
	closeDisplay d

withNextEvent :: MonadBaseControl IO m => Field -> (Event -> m a) -> m a
withNextEvent Field { display = d } act =
	liftBaseWith (\run -> allocaXEvent $ \e -> run . act =<< nextEvent d e *> getEvent e) >>= restoreM

withNextEventTimeout :: MonadBaseControl IO m => Field -> Word32 -> (Maybe Event -> m a) -> m a
withNextEventTimeout Field { display = d } n act =
	liftBaseWith (\run -> allocaXEvent $ \e -> run . act =<< do
		to <- waitForEvent d n
		(if to then pure Nothing else Just <$> (nextEvent d e >> getEvent e))) >>= restoreM

{-
withNextEventTimeout :: MonadBaseControl IO m => Field -> Int -> (Maybe Event -> m a) -> m a
withNextEventTimeout Field { display = d } n act =
	liftBaseWith (\run -> allocaXEvent $ \e -> run . act =<< do
		r <- timeout n (myPeekEvent d e) (nextEvent d e)
		maybe (pure Nothing) (const $ Just <$> getEvent e) r) >>= restoreM
		-}

fillRect :: Field ->
	Pixel -> Position -> Position -> Dimension -> Dimension -> IO ()
fillRect Field { display = dpy, pixmap = win, graphicsContext = gc } c x y w h =
	setForeground dpy gc c >> fillRectangle dpy win gc x y w h

clearField :: Field -> IO ()
clearField Field { display = dpy, pixmap = win, graphicsContext = gc, screenWidth = w, screenHeight = h } =
	setForeground dpy gc 0x000000 >> fillRectangle dpy win gc 0 0 w h -- clearWindow dpy win

flushField :: Field -> IO ()
flushField Field { display = dpy, window = win, pixmap = pxm, graphicsContext = gc, screenWidth = w, screenHeight = h } = do
	copyArea dpy pxm win gc 0 0 w h 0 0
	flush dpy
