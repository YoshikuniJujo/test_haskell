{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Data.Word
import Data.Int

import Graphics.Pango.Types
import Graphics.Pango.Values

#include <pango/pango.h>

foreign import ccall "pango_layout_new" c_pango_layout_new ::
	Ptr PangoContext -> IO (Ptr PangoLayoutOld)

pangoLayoutNew :: PangoContext -> IO PangoLayoutOld
pangoLayoutNew (PangoContext fpc) = withForeignPtr fpc \pc ->
	makePangoLayoutOld =<< c_pango_layout_new pc

foreign import ccall "pango_layout_copy" c_pango_layout_copy ::
	Ptr PangoLayoutOld -> IO (Ptr PangoLayoutOld)

pangoLayoutCopy :: PangoLayoutOld -> IO PangoLayoutOld
pangoLayoutCopy (PangoLayoutOld fpl) = withForeignPtr fpl \pl ->
	makePangoLayoutOld =<< c_pango_layout_copy pl

foreign import ccall "pango_layout_set_text" c_pango_layout_set_text ::
	Ptr PangoLayoutOld -> CString -> #{type int} -> IO ()

pangoLayoutSetText :: PangoLayoutOld -> String -> #{type int} -> IO ()
pangoLayoutSetText (PangoLayoutOld fpl) s n =
	withForeignPtr fpl \pl -> withCString s \cs ->
		c_pango_layout_set_text pl cs n

foreign import ccall "pango_layout_get_text" c_pango_layout_get_text ::
	Ptr PangoLayoutOld -> IO CString

pangoLayoutGetText :: PangoLayoutOld -> IO String
pangoLayoutGetText (PangoLayoutOld fpl) = withForeignPtr fpl \pl ->
	peekCString =<< c_pango_layout_get_text pl

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr PangoLayoutOld -> Ptr PangoFontDescription -> IO ()

pangoLayoutSetFontDescription :: PangoLayoutOld -> PangoFontDescription -> IO ()
pangoLayoutSetFontDescription (PangoLayoutOld fpl) (PangoFontDescription fpfd) =
	withForeignPtr fpl \pl -> withForeignPtr fpfd \pfd ->
		c_pango_layout_set_font_description pl pfd

foreign import ccall "pango_layout_set_width" c_pango_layout_set_width ::
	Ptr PangoLayoutOld -> #{type int} -> IO ()

pangoLayoutSetWidth :: PangoLayoutOld -> #{type int} -> IO ()
pangoLayoutSetWidth (PangoLayoutOld fpl) w = withForeignPtr fpl \pl ->
	c_pango_layout_set_width pl w

foreign import ccall "pango_layout_get_width" c_pango_layout_get_width ::
	Ptr PangoLayoutOld -> IO #type int

pangoLayoutGetWidth :: PangoLayoutOld -> IO #type int
pangoLayoutGetWidth (PangoLayoutOld fpl) =
	withForeignPtr fpl c_pango_layout_get_width

foreign import ccall "pango_layout_set_ellipsize" c_pango_layout_set_ellipsize ::
	Ptr PangoLayoutOld -> #{type PangoEllipsizeMode} -> IO ()

pangoLayoutSetEllipsize :: PangoLayoutOld -> PangoEllipsizeMode -> IO ()
pangoLayoutSetEllipsize (PangoLayoutOld fpl) (PangoEllipsizeMode pem) = withForeignPtr fpl \pl ->
	c_pango_layout_set_ellipsize pl pem

foreign import ccall "pango_layout_set_indent" c_pango_layout_set_indent ::
	Ptr PangoLayoutOld -> #{type int} -> IO ()

pangoLayoutSetIndent :: PangoLayoutOld -> #{type int} -> IO ()
pangoLayoutSetIndent (PangoLayoutOld fpl) idt = withForeignPtr fpl \pl ->
	c_pango_layout_set_indent pl idt
