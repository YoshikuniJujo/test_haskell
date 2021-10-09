{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (init)

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Data.Foldable
import Data.Word
import Numeric
import Graphics.Rendering.OpenGL
import Graphics.GL

import Lib
import TryFbo
import Juicy

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
		viewport $= (Position 0 0, Size fboWidth fboHeight)
		bindFramebuffer Framebuffer $= fb
		clearColor $= Color4 0.8 0.4 0.05 1.0
		clear [ColorBuffer]
		color (Color3 1 1 1 :: Color3 GLdouble)
		renderPrimitive LineLoop $ mapM_ vertex [
			Vertex2 (- 0.9) (- 0.9) :: Vertex2 GLdouble,
			Vertex2 (- 0.9) 0.9,
			Vertex2 0.9 0.9 ]
		flush
		bindFramebuffer Framebuffer $= defaultFramebufferObject
		allocaBytes (fromIntegral $ fboWidth * fboHeight * 4) \p -> do
			glGetTextureImage cbi 0 #{const GL_BGRA} #{const GL_UNSIGNED_INT_8_8_8_8_REV}
				(fboWidth * fboHeight * 4) p
			bsss <- groups (fromIntegral fboWidth) . groups 4
				<$> peekArray (fromIntegral $ fboWidth * fboHeight * 4) p
			print @[[[Word8]]] bsss
			iss <- groups (fromIntegral fboWidth)
				<$> peekArray (fromIntegral $ fboWidth * fboHeight) (castPtr p)
			print $ ((flip (showHex @Word32) "") <$>) <$> iss
			writeArgbPng "foo.png" iss
			for_ bsss \bss -> do
				for_ bss \cs -> do
					let	[b, g, r, a] = fromIntegral <$> cs
						x :: Int = b + g + r
					case () of
						_	| x <= 0xbf  -> putStr " "
							| x <= 0x17e -> putStr "."
							| x <= 0x23d -> putStr "+"
							| x <= 0x23d -> putStr "*"
							| otherwise -> putStr "#"
				putStrLn ""
		
		glXMakeCurrent dpy Nothing Nothing
		glXDestroyContext dpy ctx
	xCloseDisplay dpy

makeContext :: Display -> GlXContext -> IO ()
makeContext dpy ctx = pure ()

groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n xs = take n xs : groups n (drop n xs)
