{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Data.Int

import Graphics.Cairo.Types
import Graphics.Pango.Types

#include <pango/pango.h>

foreign import ccall "pango_cairo_create_layout" c_pango_cairo_create_layout ::
	Ptr (CairoT s) -> IO (Ptr PangoLayout)

pangoCairoCreateLayout :: CairoT s -> IO PangoLayout
pangoCairoCreateLayout (CairoT fcr) =
	withForeignPtr fcr \cr -> makePangoLayout =<< c_pango_cairo_create_layout cr

foreign import ccall "pango_cairo_show_layout" c_pango_cairo_show_layout ::
	Ptr (CairoT s) -> Ptr PangoLayout -> IO ()

pangoCairoShowLayout :: CairoT s -> PangoLayout -> IO ()
pangoCairoShowLayout (CairoT fcr) (PangoLayout fpl) =
	withForeignPtr fcr \cr -> withForeignPtr fpl \pl ->
		c_pango_cairo_show_layout cr pl

foreign import ccall "pango_layout_set_text" c_pango_layout_set_text ::
	Ptr PangoLayout -> CString -> #{type int} -> IO ()

pangoLayoutSetText :: PangoLayout -> String -> #{type int} -> IO ()
pangoLayoutSetText (PangoLayout fpl) s n =
	withForeignPtr fpl \pl -> withCString s \cs ->
		c_pango_layout_set_text pl cs n

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr PangoLayout -> Ptr PangoFontDescription -> IO ()

pangoLayoutSetFontDescription :: PangoLayout -> PangoFontDescription -> IO ()
pangoLayoutSetFontDescription (PangoLayout fpl) (PangoFontDescription fpfd) =
	withForeignPtr fpl \pl -> withForeignPtr fpfd \pfd ->
		c_pango_layout_set_font_description pl pfd
