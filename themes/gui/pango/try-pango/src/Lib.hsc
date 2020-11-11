{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive

import Graphics.Cairo.Types

import Graphics.Pango.Monad
import Graphics.Pango.Types

#include <pango/pango.h>

foreign import ccall "pango_cairo_create_layout" c_pango_cairo_create_layout ::
	Ptr (CairoT s) -> IO (Ptr PangoLayoutIo)

pangoCairoCreateLayout :: CairoT s -> IO PangoLayoutIo
pangoCairoCreateLayout (CairoT fcr) =
	withForeignPtr fcr \cr -> makePangoLayoutIo =<< c_pango_cairo_create_layout cr

foreign import ccall "pango_cairo_show_layout" c_pango_cairo_show_layout ::
	Ptr (CairoT s) -> Ptr PangoLayout -> IO ()

pangoCairoShowLayout :: PrimMonad m => CairoT (PrimState m) -> PangoLayout -> m ()
pangoCairoShowLayout (CairoT fcr) (PangoLayout fpl) = unPrimIo
	$ withForeignPtr fcr \cr -> withForeignPtr fpl \pl ->
		c_pango_cairo_show_layout cr pl
