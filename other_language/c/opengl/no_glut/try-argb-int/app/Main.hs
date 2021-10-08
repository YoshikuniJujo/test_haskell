{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Lib

main :: IO ()
main = do
	dpy <- xOpenDisplay Nothing
	print dpy
	print $ xDefaultScreen dpy
	glXChooseVisualWith dpy (xDefaultScreen dpy) defaultGlxAttributes print
	glXChooseVisualWith dpy (xDefaultScreen dpy) defaultGlxAttributes {
		glxRgba = True, glxDoublebuffer = True,
		glxRedSize = 1, glxGreenSize = 1, glxBlueSize = 1, glxDepthSize = 1 }
		print
	xCloseDisplay dpy

makeContext :: Display -> GlxContext -> IO ()
makeContext dpy ctx = pure ()
