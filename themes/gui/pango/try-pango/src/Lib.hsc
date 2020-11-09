{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Control.Monad.Primitive
import Data.Int

import Graphics.Cairo.Types
import Graphics.Pango.Monad
import Graphics.Pango.Types

#include <pango/pango.h>

foreign import ccall "pango_cairo_create_layout" c_pango_cairo_create_layout ::
	Ptr (CairoT s) -> IO (Ptr (PangoLayout s))

pangoCairoCreateLayout :: PrimMonad m => CairoT (PrimState m) -> m (PangoLayout (PrimState m))
pangoCairoCreateLayout (CairoT fcr) = unPrimIo
	$ withForeignPtr fcr \cr -> makePangoLayout =<< c_pango_cairo_create_layout cr

foreign import ccall "pango_cairo_show_layout" c_pango_cairo_show_layout ::
	Ptr (CairoT s) -> Ptr (PangoLayout s) -> IO ()

pangoCairoShowLayout :: PrimMonad m => CairoT (PrimState m) -> PangoLayout (PrimState m) -> m ()
pangoCairoShowLayout (CairoT fcr) (PangoLayout fpl) = unPrimIo
	$ withForeignPtr fcr \cr -> withForeignPtr fpl \pl ->
		c_pango_cairo_show_layout cr pl

foreign import ccall "pango_layout_set_text" c_pango_layout_set_text ::
	Ptr (PangoLayout s) -> CString -> #{type int} -> IO ()

pangoLayoutSetText :: PrimMonad m =>
	PangoLayout (PrimState m) -> String -> #{type int} -> m ()
pangoLayoutSetText (PangoLayout fpl) s n = unPrimIo
	$ withForeignPtr fpl \pl -> withCString s \cs ->
		c_pango_layout_set_text pl cs n

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr (PangoLayout s) -> Ptr PangoFontDescription -> IO ()

pangoLayoutSetFontDescription :: PangoLayout s -> PangoFontDescription -> IO ()
pangoLayoutSetFontDescription (PangoLayout fpl) (PangoFontDescription fpfd) =
	withForeignPtr fpl \pl -> withForeignPtr fpfd \pfd ->
		c_pango_layout_set_font_description pl pfd
