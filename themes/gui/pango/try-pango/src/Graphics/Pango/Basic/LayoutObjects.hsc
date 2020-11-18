{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Control.Monad.Primitive
import Data.Word
import Data.Int
import System.IO.Unsafe

import Graphics.Pango.Monad
import Graphics.Pango.Types
import Graphics.Pango.Values
import System.Glib.SinglyLinkedLists

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

foreign import ccall "pango_layout_get_size" c_pango_layout_get_size ::
	Ptr PangoLayout -> Ptr #{type int} -> Ptr #{type int} -> IO ()

pangoLayoutGetSize :: PangoLayout -> (#{type int}, #{type int})
pangoLayoutGetSize (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl \pl -> alloca \w -> alloca \h -> do
		c_pango_layout_get_size pl w h
		(,) <$> peek w <*> peek h

foreign import ccall "pango_layout_get_pixel_size" c_pango_layout_get_pixel_size ::
	Ptr PangoLayout -> Ptr #{type int} -> Ptr #{type int} -> IO ()

pangoLayoutGetPixelSize :: PangoLayout -> (#{type int}, #{type int})
pangoLayoutGetPixelSize (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl \pl -> alloca \w -> alloca \h -> do
		c_pango_layout_get_pixel_size pl w h
		(,) <$> peek w <*> peek h

foreign import ccall "pango_layout_get_baseline" c_pango_layout_get_baseline ::
	Ptr PangoLayout -> IO #type int

pangoLayoutGetBaseline :: PangoLayout -> #type int
pangoLayoutGetBaseline (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl c_pango_layout_get_baseline

foreign import ccall "pango_layout_get_line_count" c_pango_layout_get_line_count ::
	Ptr PangoLayout -> IO #type int

pangoLayoutGetLineCount :: PangoLayout -> #type int
pangoLayoutGetLineCount (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl c_pango_layout_get_line_count

foreign import ccall "pango_layout_get_line_readonly" c_pango_layout_get_line_readonly ::
	Ptr PangoLayout -> #{type int} -> IO (Ptr PangoLayoutLine)

pangoLayoutGetLine :: PangoLayout -> #{type int} -> PangoLayoutLine
pangoLayoutGetLine (PangoLayout fpl) ln = unsafePerformIO
	$ makePangoLayoutLine0 =<< withForeignPtr fpl \pl -> c_pango_layout_get_line_readonly pl ln

foreign import ccall "pango_layout_line_get_extents" c_pango_layout_line_get_extents ::
	Ptr PangoLayoutLine -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutLineGetExtents :: PangoLayoutLine -> (PangoRectangle, PangoRectangle)
pangoLayoutLineGetExtents (PangoLayoutLine fpll) = unsafePerformIO
	$ withForeignPtr fpll \pll -> alloca \irct -> alloca \lrct -> do
		c_pango_layout_line_get_extents pll irct lrct
		(,) <$> peek irct <*> peek lrct

foreign import ccall "pango_layout_line_get_pixel_extents" c_pango_layout_line_get_pixel_extents ::
	Ptr PangoLayoutLine -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutLineGetPixelExtents :: PangoLayoutLine -> (PangoRectangle, PangoRectangle)
pangoLayoutLineGetPixelExtents (PangoLayoutLine fpll) = unsafePerformIO
	$ withForeignPtr fpll \pll -> alloca \irct -> alloca \lrct -> do
		c_pango_layout_line_get_pixel_extents pll irct lrct
		(,) <$> peek irct <*> peek lrct

foreign import ccall "pango_layout_get_lines_readonly" c_pango_layout_get_lines_readonly ::
	Ptr PangoLayout -> IO (Ptr (GSList PangoLayoutLine))

pangoLayoutGetLines :: PangoLayout -> [PangoLayoutLine]
pangoLayoutGetLines (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl \pl ->
		mapM makePangoLayoutLine0 =<< g_slist_to_list =<< c_pango_layout_get_lines_readonly pl

foreign import ccall "pango_layout_get_iter" c_pango_layout_get_iter ::
	Ptr PangoLayout -> IO (Ptr (PangoLayoutIter s))

pangoLayoutGetIter :: PrimMonad m => PangoLayout -> m (PangoLayoutIter (PrimState m))
pangoLayoutGetIter (PangoLayout fpl) = unPrimIo
	$ makePangoLayoutIter =<< withForeignPtr fpl c_pango_layout_get_iter

foreign import ccall "pango_layout_iter_next_run" c_pango_layout_iter_next_run ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterNextRun :: PrimMonad m => PangoLayoutIter (PrimState m) -> m Bool
pangoLayoutIterNextRun (PangoLayoutIter fpli) = unPrimIo
	$ gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_run

foreign import ccall "pango_layout_iter_next_char" c_pango_layout_iter_next_char ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterNextChar :: PrimMonad m => PangoLayoutIter (PrimState m) -> m Bool
pangoLayoutIterNextChar (PangoLayoutIter fpli) = unPrimIo
	$ gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_char

foreign import ccall "pango_layout_iter_get_run_readonly" c_pango_layout_iter_get_run_readonly ::
	Ptr (PangoLayoutIter s) -> IO (Ptr PangoLayoutRun)

pangoLayoutIterGetRun :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m PangoLayoutRun
pangoLayoutIterGetRun (PangoLayoutIter fpli) = unPrimIo
	$ makePangoGlyphItem0 =<< withForeignPtr fpli c_pango_layout_iter_get_run_readonly

foreign import ccall "pango_layout_iter_get_index" c_pango_layout_iter_get_index ::
	Ptr (PangoLayoutIter s) -> IO #type int

pangoLayoutIterGetIndex :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m #type int
pangoLayoutIterGetIndex (PangoLayoutIter fpli) = unPrimIo
	$ withForeignPtr fpli c_pango_layout_iter_get_index
