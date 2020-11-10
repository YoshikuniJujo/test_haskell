{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Data.Int

import Graphics.Pango.Types

foreign import ccall "pango_layout_new" c_pango_layout_new ::
	Ptr PangoContext -> IO (Ptr PangoLayout)

pangoLayoutNew :: PangoContext -> IO PangoLayout
pangoLayoutNew (PangoContext fpc) = withForeignPtr fpc \pc ->
	makePangoLayout =<< c_pango_layout_new pc

foreign import ccall "pango_layout_copy" c_pango_layout_copy ::
	Ptr PangoLayout -> IO (Ptr PangoLayout)

pangoLayoutCopy :: PangoLayout -> IO PangoLayout
pangoLayoutCopy (PangoLayout fpl) = withForeignPtr fpl \pl ->
	makePangoLayout =<< c_pango_layout_copy pl

foreign import ccall "pango_layout_set_text" c_pango_layout_set_text ::
	Ptr PangoLayout -> CString -> #{type int} -> IO ()

pangoLayoutSetText :: PangoLayout -> String -> #{type int} -> IO ()
pangoLayoutSetText (PangoLayout fpl) s n =
	withForeignPtr fpl \pl -> withCString s \cs ->
		c_pango_layout_set_text pl cs n

foreign import ccall "pango_layout_get_text" c_pango_layout_get_text ::
	Ptr PangoLayout -> IO CString

pangoLayoutGetText :: PangoLayout -> IO String
pangoLayoutGetText (PangoLayout fpl) = withForeignPtr fpl \pl ->
	peekCString =<< c_pango_layout_get_text pl

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr PangoLayout -> Ptr PangoFontDescription -> IO ()

pangoLayoutSetFontDescription :: PangoLayout -> PangoFontDescription -> IO ()
pangoLayoutSetFontDescription (PangoLayout fpl) (PangoFontDescription fpfd) =
	withForeignPtr fpl \pl -> withForeignPtr fpfd \pfd ->
		c_pango_layout_set_font_description pl pfd

foreign import ccall "pango_layout_set_width" c_pango_layout_set_width ::
	Ptr PangoLayout -> #{type int} -> IO ()

pangoLayoutSetWidth :: PangoLayout -> #{type int} -> IO ()
pangoLayoutSetWidth (PangoLayout fpl) w = withForeignPtr fpl \pl ->
	c_pango_layout_set_width pl w

foreign import ccall "pango_layout_get_width" c_pango_layout_get_width ::
	Ptr PangoLayout -> IO #type int

pangoLayoutGetWidth :: PangoLayout -> IO #type int
pangoLayoutGetWidth (PangoLayout fpl) =
	withForeignPtr fpl c_pango_layout_get_width
