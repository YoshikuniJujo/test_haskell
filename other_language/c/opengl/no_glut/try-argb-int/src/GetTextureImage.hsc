{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GetTextureImage (render) where

import Prelude hiding (init)

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.C.Types
import Data.Word
import Data.CairoImage.Internal
import Graphics.Rendering.OpenGL
import Graphics.GL

import Lib

#include <GL/gl.h>

type Format = GLenum
type Type = GLenum
type BufSize = GLsizei

render :: Size -> IO () -> IO Argb32
render s@(Size w h) dr = do
	dpy <- xOpenDisplay Nothing
	argb' <- glXChooseVisualWith dpy (xDefaultScreen dpy) defaultGlxAttributes {
		glxRgba = True, glxDoublebuffer = True,
		glxRedSize = 1, glxGreenSize = 1, glxBlueSize = 1, glxDepthSize = 1 } \v -> do
		ctx <- glXCreateContext dpy v Nothing True
		True <- glXMakeCurrent dpy Nothing (Just ctx)
		(fb, cb) <- init w h
		viewport $= (Position 0 0, s)
		bindFramebuffer Framebuffer $= fb

		dr

		flush

		bindFramebuffer Framebuffer $= defaultFramebufferObject

		p <- mallocBytes (fromIntegral $ w * h * 4)

		getTextureImage cb 0 glBgra glUnsignedInt_8_8_8_8_rev
			(w * h * 4) p
		argb <- makeArgb32 (fromIntegral w) (fromIntegral h) p

		True <- glXMakeCurrent dpy Nothing Nothing
		glXDestroyContext dpy ctx

		pure argb
	argb' <$ xCloseDisplay dpy

makeArgb32 :: CInt -> CInt -> Ptr Word32 -> IO Argb32
makeArgb32 w h p = getArgb32 <$> do
	fp <- newForeignPtr p' $ free p'
	pure $ CairoImage CairoFormatArgb32 w h (w * 4) fp
	where
	p' = castPtr p
	getArgb32 = \case CairoImageArgb32 i -> i; _ -> error "not Argb32"
		

getTextureImage ::
	TextureObject -> Level -> Format -> Type -> BufSize -> Ptr a -> IO ()
getTextureImage (TextureObject cbi) lvl fmt tp sz p =
	glGetTextureImage cbi lvl fmt tp sz p

glBgra, glUnsignedInt_8_8_8_8_rev :: GLenum
glBgra = #{const GL_BGRA}
glUnsignedInt_8_8_8_8_rev = #{const GL_UNSIGNED_INT_8_8_8_8_REV}

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
