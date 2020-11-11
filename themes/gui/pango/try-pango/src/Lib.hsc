{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.ForeignPtr

import Graphics.Cairo.Types
import Graphics.Pango.Types

#include <pango/pango.h>

foreign import ccall "pango_cairo_create_layout" c_pango_cairo_create_layout ::
	Ptr (CairoT s) -> IO (Ptr PangoLayoutOld)

pangoCairoCreateLayout :: CairoT s -> IO PangoLayoutOld
pangoCairoCreateLayout (CairoT fcr) =
	withForeignPtr fcr \cr -> makePangoLayoutOld =<< c_pango_cairo_create_layout cr

foreign import ccall "pango_cairo_show_layout" c_pango_cairo_show_layout ::
	Ptr (CairoT s) -> Ptr PangoLayoutOld -> IO ()

pangoCairoShowLayout :: CairoT s -> PangoLayoutOld -> IO ()
pangoCairoShowLayout (CairoT fcr) (PangoLayoutOld fpl) =
	withForeignPtr fcr \cr -> withForeignPtr fpl \pl ->
		c_pango_cairo_show_layout cr pl
