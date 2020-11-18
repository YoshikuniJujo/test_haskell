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

foreign import ccall "pango_layout_xy_to_index" c_pango_layout_xy_to_index ::
	Ptr PangoLayout -> #{type int} -> #{type int} -> Ptr #{type int} -> Ptr #{type int} -> IO #type gboolean

pangoLayoutXyToIndex :: PangoLayout -> #{type int} -> #{type int} -> (#{type int}, #{type int}, Bool)
pangoLayoutXyToIndex (PangoLayout fpl) x y = unsafePerformIO
	$ withForeignPtr fpl \pl -> alloca \idx -> alloca \tr -> do
		isd <- c_pango_layout_xy_to_index pl x y idx tr
		(,,) <$> peek idx <*> peek tr <*> pure (gbooleanToBool isd)

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

foreign import ccall "pango_layout_get_cursor_pos" c_pango_layout_get_cursor_pos ::
	Ptr PangoLayout -> #{type int} -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutGetCursorPos :: PangoLayout -> #{type int} -> (PangoRectangle, PangoRectangle)
pangoLayoutGetCursorPos (PangoLayout fpl) idx = unsafePerformIO
	$ withForeignPtr fpl \pl -> alloca \spos -> alloca \wpos -> do
		c_pango_layout_get_cursor_pos pl idx spos wpos
		(,) <$> peek spos <*> peek wpos

foreign import ccall "pango_layout_move_cursor_visually" c_pango_layout_move_cursor_visually ::
	Ptr PangoLayout -> #{type gboolean} -> #{type int} -> #{type int} -> #{type int} ->
	Ptr #{type int} -> Ptr #{type int} -> IO ()

pangoLayoutMoveCursorVisually ::
	PangoLayout -> Bool -> #{type int} -> #{type int} -> #{type int} -> (#{type int}, #{type int})
pangoLayoutMoveCursorVisually (PangoLayout fpl) str oidx otr dir = unsafePerformIO
	$ withForeignPtr fpl \pl -> alloca \nidx -> alloca \ntr -> do
		c_pango_layout_move_cursor_visually pl (boolToGboolean str) oidx otr dir nidx ntr
		(,) <$> peek nidx <*> peek ntr

foreign import ccall "pango_layout_get_extents" c_pango_layout_get_extents ::
	Ptr PangoLayout -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutGetExtents :: PangoLayout -> (PangoRectangle, PangoRectangle)
pangoLayoutGetExtents (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl \pl -> alloca \irct -> alloca \lrct -> do
		c_pango_layout_get_extents pl irct lrct
		(,) <$> peek irct <*> peek lrct

foreign import ccall "pango_layout_get_pixel_extents" c_pango_layout_get_pixel_extents ::
	Ptr PangoLayout -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutGetPixelExtents :: PangoLayout -> (PangoRectangle, PangoRectangle)
pangoLayoutGetPixelExtents (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl \pl -> alloca \irct -> alloca \lrct -> do
		c_pango_layout_get_pixel_extents pl irct lrct
		(,) <$> peek irct <*> peek lrct

foreign import ccall "pango_extents_to_pixels" c_pango_extents_to_pixels ::
	Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoExtentsToPixelsInclusive :: PangoRectangle -> PangoRectangle
pangoExtentsToPixelsInclusive src = unsafePerformIO
	$ alloca \dst -> do
		poke dst src
		c_pango_extents_to_pixels dst nullPtr
		peek dst

pangoExtentsToPixelsNearest :: PangoRectangle -> PangoRectangle
pangoExtentsToPixelsNearest src = unsafePerformIO
	$ alloca \dst -> do
		poke dst src
		c_pango_extents_to_pixels nullPtr dst
		peek dst
