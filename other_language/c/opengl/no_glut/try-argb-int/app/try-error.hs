{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Lib

main :: IO ()
main = do
	dpy <- xOpenDisplay Nothing
	let	scr = xDefaultScreen dpy
	print dpy
	print scr
	glXChooseVisualWith dpy scr defaultGlxAttributes {
		glxRgba = True, glxDoublebuffer = True,
		glxRedSize = 1, glxGreenSize = 1, glxBlueSize = 1, glxDepthSize = 1 } \v -> do
		print v
		ctx <- glXCreateContext dpy v Nothing True
		print ctx
		True <- glXMakeCurrent dpy Nothing (Just ctx)
		True <- glXMakeCurrent dpy Nothing Nothing
		pure ()
	xCloseDisplay dpy
