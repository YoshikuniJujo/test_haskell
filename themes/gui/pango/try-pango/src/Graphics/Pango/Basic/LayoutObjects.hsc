{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Data.Word
import Data.Int
import System.IO.Unsafe

import Graphics.Pango.Types
import Graphics.Pango.Values

#include <pango/pango.h>

foreign import ccall "pango_layout_new" c_pango_layout_new ::
	Ptr PangoContext -> IO (Ptr PangoLayoutIo)

pangoLayoutNew :: PangoContext -> IO PangoLayoutIo
pangoLayoutNew (PangoContext fpc) = withForeignPtr fpc \pc ->
	makePangoLayoutIo =<< c_pango_layout_new pc

foreign import ccall "pango_layout_copy" c_pango_layout_copy ::
	Ptr PangoLayoutIo -> IO (Ptr PangoLayoutIo)

pangoLayoutCopy :: PangoLayoutIo -> IO PangoLayoutIo
pangoLayoutCopy (PangoLayoutIo fpl) = withForeignPtr fpl \pl ->
	makePangoLayoutIo =<< c_pango_layout_copy pl

foreign import ccall "pango_layout_set_text" c_pango_layout_set_text ::
	Ptr PangoLayoutIo -> CString -> #{type int} -> IO ()

pangoLayoutSetText :: PangoLayoutIo -> String -> #{type int} -> IO ()
pangoLayoutSetText (PangoLayoutIo fpl) s n =
	withForeignPtr fpl \pl -> withCString s \cs ->
		c_pango_layout_set_text pl cs n

foreign import ccall "pango_layout_get_text" c_pango_layout_get_text ::
	Ptr PangoLayout -> IO CString

pangoLayoutGetText :: PangoLayout -> String
pangoLayoutGetText (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl \pl -> peekCString =<< c_pango_layout_get_text pl

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr PangoLayoutIo -> Ptr PangoFontDescription -> IO ()

pangoLayoutSetFontDescription :: PangoLayoutIo -> PangoFontDescription -> IO ()
pangoLayoutSetFontDescription (PangoLayoutIo fpl) (PangoFontDescription fpfd) =
	withForeignPtr fpl \pl -> withForeignPtr fpfd \pfd ->
		c_pango_layout_set_font_description pl pfd

foreign import ccall "pango_layout_set_width" c_pango_layout_set_width ::
	Ptr PangoLayoutIo -> #{type int} -> IO ()

pangoLayoutSetWidth :: PangoLayoutIo -> #{type int} -> IO ()
pangoLayoutSetWidth (PangoLayoutIo fpl) w = withForeignPtr fpl \pl ->
	c_pango_layout_set_width pl w

foreign import ccall "pango_layout_get_width" c_pango_layout_get_width ::
	Ptr PangoLayout -> IO #type int

pangoLayoutGetWidth :: PangoLayout -> #type int
pangoLayoutGetWidth (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl c_pango_layout_get_width

foreign import ccall "pango_layout_set_ellipsize" c_pango_layout_set_ellipsize ::
	Ptr PangoLayoutIo -> #{type PangoEllipsizeMode} -> IO ()

pangoLayoutSetEllipsize :: PangoLayoutIo -> PangoEllipsizeMode -> IO ()
pangoLayoutSetEllipsize (PangoLayoutIo fpl) (PangoEllipsizeMode pem) = withForeignPtr fpl \pl ->
	c_pango_layout_set_ellipsize pl pem

foreign import ccall "pango_layout_set_indent" c_pango_layout_set_indent ::
	Ptr PangoLayoutIo -> #{type int} -> IO ()

pangoLayoutSetIndent :: PangoLayoutIo -> #{type int} -> IO ()
pangoLayoutSetIndent (PangoLayoutIo fpl) idt = withForeignPtr fpl \pl ->
	c_pango_layout_set_indent pl idt

{-
foreign import ccall "pango_layout_set_line_spacing" c_pango_layout_set_line_spacing ::
	Ptr PangoLayoutIo -> #{type float} -> IO ()

pangoLayoutSetLineSpacing :: PangoLayoutIo -> #{type float} -> IO ()
pangoLayoutSetLineSpacing (PangoLayoutIo fpl) fct = withForeignPtr fpl \pl ->
	c_pango_layout_set_line_spacing pl fct
	-}

foreign import ccall "pango_layout_set_alignment" c_pango_layout_set_alignment ::
	Ptr PangoLayoutIo -> #{type PangoAlignment} -> IO ()

pangoLayoutSetAlignment :: PangoLayoutIo -> PangoAlignment -> IO ()
pangoLayoutSetAlignment (PangoLayoutIo fpl) (PangoAlignment pa) = withForeignPtr fpl \pl ->
	c_pango_layout_set_alignment pl pa

foreign import ccall "pango_layout_set_tabs" c_pango_layout_set_tabs ::
	Ptr PangoLayoutIo -> Ptr PangoTabArray -> IO ()

pangoLayoutSetTabs :: PangoLayoutIo -> PangoTabArray -> IO ()
pangoLayoutSetTabs (PangoLayoutIo fpl) (PangoTabArray fpta) =
	withForeignPtr fpl \pl ->
		withForeignPtr fpta \pta -> c_pango_layout_set_tabs pl pta

foreign import ccall "pango_layout_set_single_paragraph_mode"
	c_pango_layout_set_single_paragraph_mode ::
	Ptr PangoLayoutIo -> #{type gboolean} -> IO ()

pangoLayoutSetSingleParagraphMode :: PangoLayoutIo -> Bool -> IO ()
pangoLayoutSetSingleParagraphMode (PangoLayoutIo fpl) spm =
	withForeignPtr fpl \pl ->
		c_pango_layout_set_single_paragraph_mode pl (boolToGboolean spm)

boolToGboolean :: Bool -> #type gboolean
boolToGboolean False = #const FALSE
boolToGboolean True = #const TRUE

foreign import ccall "pango_layout_get_unknown_glyphs_count"
	c_pango_layout_get_unknown_glyphs_count :: Ptr PangoLayout -> IO #type int

pangoLayoutGetUnknownGlyphsCount :: PangoLayout -> #type int
pangoLayoutGetUnknownGlyphsCount (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl c_pango_layout_get_unknown_glyphs_count

foreign import ccall "pango_layout_index_to_pos" c_pango_layout_index_to_pos ::
	Ptr PangoLayout -> #{type int} -> Ptr PangoRectangle -> IO ()

pangoLayoutIndexToPos :: PangoLayout -> #{type int} -> PangoRectangle
pangoLayoutIndexToPos (PangoLayout fpl) idx = unsafePerformIO
	$ withForeignPtr fpl \pl -> alloca \pos -> do
		c_pango_layout_index_to_pos pl idx pos
		peek pos

foreign import ccall "pango_layout_index_to_line_x"
	c_pango_layout_index_to_line_x ::
	Ptr PangoLayout -> #{type int} -> #{type gboolean} -> Ptr #{type int} -> Ptr #{type int} -> IO ()

pangoLayoutIndexToLineX :: PangoLayout -> #{type int} -> Bool -> (#{type int}, #{type int})
pangoLayoutIndexToLineX (PangoLayout fpl) idx tr = unsafePerformIO
	$ withForeignPtr fpl \pl -> alloca \ln -> alloca \xpos -> do
		c_pango_layout_index_to_line_x pl idx (boolToGboolean tr) ln xpos
		(,) <$> peek ln <*> peek xpos
