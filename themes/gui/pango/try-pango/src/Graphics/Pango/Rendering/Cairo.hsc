{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Rendering.Cairo where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.ST
import Graphics.Cairo.Types

import Graphics.Pango.Types

foreign import ccall "pango_cairo_create_context"
	c_pango_cairo_create_context :: Ptr (CairoT s) -> IO (Ptr PangoContextOld)

pangoCairoCreateContext :: CairoT RealWorld -> IO PangoContextOld
pangoCairoCreateContext (CairoT fcr) = withForeignPtr fcr \cr ->
	makePangoContextOld =<< c_pango_cairo_create_context cr
