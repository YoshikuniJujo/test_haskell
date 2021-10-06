{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (init)

import Foreign.Ptr
import Control.Monad
import Data.Bool
import Data.IORef
import Graphics.UI.GLUT
import System.Environment

fboWidth, fboHeight :: GLsizei
fboWidth = 512; fboHeight = 512

main :: IO ()
main = do
	wdt <- newIORef 0
	hgt <- newIORef 0
	pn <- getProgName
	void $ initialize pn =<< getArgs
	void $ createWindow pn
	reshapeCallback $= Just \(Size w h) ->
		writeIORef wdt w >> writeIORef hgt h
	keyboardCallback $= Just \c p ->
		print (c, p) >> bool (return ()) leaveMainLoop (c == 'q')
	(fb, cb) <- init
	displayCallback $= display fb cb wdt hgt
	mainLoop

display :: FramebufferObject -> TextureObject -> IORef GLsizei -> IORef GLsizei -> DisplayCallback
display fb cb wdt hgt = do
	matrixMode $= Projection
	loadIdentity
	matrixMode $= Modelview 0
	loadIdentity
	viewport $= (Position 0 0, Size fboWidth fboHeight)

	bindFramebuffer Framebuffer $= fb
	clear [ColorBuffer, DepthBuffer]
	renderObject Wireframe $ Teapot 0.5
	flush
	bindFramebuffer Framebuffer $= defaultFramebufferObject

	matrixMode $= Projection
	loadIdentity
	matrixMode $= Modelview 0
	loadIdentity

	w <- readIORef wdt
	h <- readIORef hgt
	viewport $= (Position 0 0, Size w h)

	lighting $= Disabled
	depthFunc $= Nothing
	textureBinding Texture2D $= Just cb
	texture Texture2D $= Enabled

	color $ Color3 @GLdouble 1.0 1.0 1.0

	renderPrimitive TriangleFan do
		texCoord $ TexCoord2 @GLdouble 0.0 0.0
		vertex $ Vertex2 @GLdouble (- 1.0) (- 1.0)
		texCoord $ TexCoord2 @GLdouble 1.0 0.0
		vertex $ Vertex2 @GLdouble 1.0 (- 1.0)
		texCoord $ TexCoord2 @GLdouble 1.0 1.0
		vertex $ Vertex2 @GLdouble 1.0 1.0
		texCoord $ TexCoord2 @GLdouble 0.0 1.0
		vertex $ Vertex2 @GLdouble (- 1.0) 1.0

	texture Texture2D $= Disabled
	textureBinding Texture2D $= Nothing

	flush

init :: IO (FramebufferObject, TextureObject)
init = do
	cb <- genObjectName
	textureBinding Texture2D $= Just cb
	texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D fboWidth fboHeight)
		0 (PixelData RGBA UnsignedByte nullPtr)
	textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
	textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
	textureFilter Texture2D $= ((Linear', Nothing), Linear')
	textureBinding Texture2D $= Nothing

	rb <- genObjectName
	bindRenderbuffer Renderbuffer $= rb
	renderbufferStorage Renderbuffer DepthComponent' $ RenderbufferSize fboWidth fboHeight
	bindRenderbuffer Renderbuffer $= noRenderbufferObject

	fb <- genObjectName
	bindFramebuffer Framebuffer $= fb
	framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D cb 0
	framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer rb
	bindFramebuffer Framebuffer $= defaultFramebufferObject

	pure (fb, cb)
