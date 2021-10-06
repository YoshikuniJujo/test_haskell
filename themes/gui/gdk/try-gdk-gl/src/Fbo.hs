{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Fbo where

import Foreign.Ptr
import Data.IORef
import System.Environment
import Graphics.UI.GLUT

initializeGlut :: IO (String, [String])
initializeGlut = do
	pn <- getProgName
	(pn ,) <$> (initialize pn =<< getArgs)

drawFramebuffer :: FramebufferObject -> GLsizei -> GLsizei -> DisplayCallback
drawFramebuffer fb fw fh = do
	matrixMode $= Projection
	loadIdentity
	matrixMode $= Modelview 0
	loadIdentity
	viewport $= (Position 0 0, Size fw fh)

	bindFramebuffer Framebuffer $= fb
	clear [ColorBuffer, DepthBuffer]
	renderObject Wireframe $ Teapot 0.5
	flush
	bindFramebuffer Framebuffer $= defaultFramebufferObject

drawTexture :: TextureObject -> IORef GLsizei -> IORef GLsizei -> DisplayCallback
drawTexture cb wdt hgt = do
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

initializeFbo :: GLsizei -> GLsizei -> IO (FramebufferObject, TextureObject)
initializeFbo w h = do
	cb <- genObjectName
	textureBinding Texture2D $= Just cb
	texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D w h)
		0 (PixelData RGBA UnsignedByte nullPtr)
	textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
	textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
	textureFilter Texture2D $= ((Linear', Nothing), Linear')
	textureBinding Texture2D $= Nothing

	rb <- genObjectName
	bindRenderbuffer Renderbuffer $= rb
	renderbufferStorage Renderbuffer DepthComponent' $ RenderbufferSize w h
	bindRenderbuffer Renderbuffer $= noRenderbufferObject

	fb <- genObjectName
	bindFramebuffer Framebuffer $= fb
	framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D cb 0
	framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer rb
	bindFramebuffer Framebuffer $= defaultFramebufferObject

	pure (fb, cb)
