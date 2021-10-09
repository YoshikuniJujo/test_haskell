{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (init)

import Foreign.Storable
import Foreign.Marshal
import Data.Word
import Graphics.Rendering.OpenGL
import Graphics.GL

import Lib
import TryFbo

#include <GL/glx.h>

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
		(fb, cb@(TextureObject cbi)) <- init
		bindFramebuffer Framebuffer $= fb
		clearColor $= Color4 0.8 0.4 0.05 1.0
		clear [ColorBuffer]
		flush
		bindFramebuffer Framebuffer $= defaultFramebufferObject
		allocaBytes (fromIntegral $ fboWidth * fboHeight * 4) \p -> do
			glGetTextureImage cbi 0 #{const GL_BGRA} #{const GL_UNSIGNED_INT_8_8_8_8_REV}
				(fboWidth * fboHeight * 4) p
			print @[Word8] =<< peekArray (fromIntegral $ fboWidth * fboHeight * 4) p
		
		glXMakeCurrent dpy Nothing Nothing
		glXDestroyContext dpy ctx
	xCloseDisplay dpy

makeContext :: Display -> GlXContext -> IO ()
makeContext dpy ctx = pure ()
