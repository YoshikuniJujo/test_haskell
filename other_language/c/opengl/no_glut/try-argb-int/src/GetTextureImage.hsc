{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GetTextureImage where

import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Graphics.GL

#include <GL/gl.h>

type Format = GLenum
type Type = GLenum
type BufSize = GLsizei

getTextureImage ::
	TextureObject -> Level -> Format -> Type -> BufSize -> Ptr a -> IO ()
getTextureImage (TextureObject cbi) lvl fmt tp sz p =
	glGetTextureImage cbi lvl fmt tp sz p

glBgra, glUnsignedInt_8_8_8_8_rev :: GLenum
glBgra = #{const GL_BGRA}
glUnsignedInt_8_8_8_8_rev = #{const GL_UNSIGNED_INT_8_8_8_8_REV}
