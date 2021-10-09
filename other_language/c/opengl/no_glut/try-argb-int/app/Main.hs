{-# LANGUAGE BlockArguments #-}
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
		glxRedSize = 1, glxGreenSize = 1, glxBlueSize = 1, glxDepthSize = 1 } \v -> do
		print v
		ctx <- glXCreateContext dpy v Nothing True
		print ctx
		glXMakeCurrent dpy Nothing (Just ctx)
		glXMakeCurrent dpy Nothing Nothing
		glXDestroyContext dpy ctx
	xCloseDisplay dpy

makeContext :: Display -> GlXContext -> IO ()
makeContext dpy ctx = pure ()
