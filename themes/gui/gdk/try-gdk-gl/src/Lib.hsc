{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.C.Types

import Data.CairoContext
import Graphics.Gdk.Windows

#include <GL/glut.h>

foreign import ccall "gdk_cairo_draw_from_gl" c_gdk_cairo_draw_from_gl ::
	Ptr (CairoT s t) -> Ptr GdkWindow ->
		CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

glRenderbuffer, glTexture :: CInt
glRenderbuffer = #{const GL_RENDERBUFFER}
glTexture = #{const GL_TEXTURE}
