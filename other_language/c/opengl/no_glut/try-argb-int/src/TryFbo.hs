{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryFbo where

import Foreign.Ptr
import Graphics.Rendering.OpenGL

init :: GLsizei -> GLsizei -> IO (FramebufferObject, TextureObject)
init w h = do
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
