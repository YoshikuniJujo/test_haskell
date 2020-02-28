{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.Field.DrawString where

import Numeric
import Graphics.X11
import Graphics.X11.Xft
import Graphics.X11.Xrender

import Field.Internal

drawStr :: Field -> Pixel -> Position -> Position -> String -> IO ()
drawStr Field { display = dpy, pixmap = win, graphicsContext = gc } c x y s =
	setForeground dpy gc c >> drawString dpy win gc x y s

writeString :: Field -> String -> Double -> Position -> Position -> String -> IO ()
writeString Field { display = dpy, pixmap = win } fnt sz x y str = do
	let	vsl = defaultVisual dpy $ defaultScreen dpy
		cm = defaultColormap dpy $ defaultScreen dpy
	draw <- xftDrawCreate dpy win vsl cm
	font <- xftFontOpen dpy (defaultScreenOfDisplay dpy) $
		fnt ++ "-" ++ showFFloat (Just 0) sz ""
	withXftColorValue dpy vsl cm XRenderColor {
		xrendercolor_red = 0xffff,
		xrendercolor_blue = 0x0000,
		xrendercolor_green = 0xffff,
		xrendercolor_alpha = 0xffff } \c ->
		xftDrawString draw c font x y str

sample :: IO ()
sample = do
	f <- openField "ゴシック体と明朝体" []
	writeString f "Sazanami Gothic" 45 10 100 "ゴシックなので、明朝体でない。"
	writeString f "Sazanami Mincho" 45 10 180 "ゴシックでなく、明朝体ですよ。"
	writeString f "Sazanami Gothic" 100 100 320 "永遠"
	writeString f "Sazanami Mincho" 100 100 480 "永遠"
	flushField f
	_ <- getLine
	closeField f
