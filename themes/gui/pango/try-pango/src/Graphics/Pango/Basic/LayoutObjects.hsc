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

import Graphics.Pango.Types
import Graphics.Pango.Values
import System.Glib.SinglyLinkedLists

import Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim

import Graphics.Pango.Basic.Rendering

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

#include <pango/pango.h>

foreign import ccall "pango_layout_new" c_pango_layout_new ::
	Ptr PangoContext -> IO (Ptr (PangoLayoutPrim s))

pangoLayoutNew :: PangoContext -> IO (PangoLayoutPrim RealWorld)
pangoLayoutNew (PangoContext fpc) = unsafeIOToPrim $ withForeignPtr fpc \pc ->
	mkPangoLayoutPrim =<< c_pango_layout_new pc

pangoLayoutSetText :: PrimMonad m => PangoLayoutPrim (PrimState m) -> String -> #{type int} -> m ()
pangoLayoutSetText (PangoLayoutPrim fpl) s n = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> withCString s \cs ->
		c_pango_layout_set_text pl cs n

foreign import ccall "pango_layout_set_text" c_pango_layout_set_text ::
	Ptr (PangoLayoutPrim s) -> CString -> #{type int} -> IO ()

foreign import ccall "pango_layout_get_text" c_pango_layout_get_text ::
	Ptr PangoLayout -> IO CString

pangoLayoutGetText :: PangoLayout -> String
pangoLayoutGetText (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl \pl -> peekCString =<< c_pango_layout_get_text pl

pangoLayoutSetFontDescription :: PrimMonad m => PangoLayoutPrim (PrimState m) -> PangoFontDescription (PrimState m) -> m ()
pangoLayoutSetFontDescription (PangoLayoutPrim fpl) (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> withForeignPtr fpfd \pfd ->
		c_pango_layout_set_font_description pl pfd

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr (PangoLayoutPrim s) -> Ptr (PangoFontDescription s) -> IO ()

pangoLayoutSetWidth :: PrimMonad m => PangoLayoutPrim (PrimState m) -> #{type int} -> m ()
pangoLayoutSetWidth (PangoLayoutPrim fpl) w = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> c_pango_layout_set_width pl w

foreign import ccall "pango_layout_set_width" c_pango_layout_set_width ::
	Ptr (PangoLayoutPrim s) -> #{type int} -> IO ()

foreign import ccall "pango_layout_get_width" c_pango_layout_get_width ::
	Ptr PangoLayout -> IO #type int

pangoLayoutGetWidth :: PangoLayout -> #type int
pangoLayoutGetWidth (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl c_pango_layout_get_width

pangoLayoutSetEllipsize :: PrimMonad m => PangoLayoutPrim (PrimState m) -> PangoEllipsizeMode -> m ()
pangoLayoutSetEllipsize (PangoLayoutPrim fpl) (PangoEllipsizeMode pem) = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> c_pango_layout_set_ellipsize pl pem

foreign import ccall "pango_layout_set_ellipsize" c_pango_layout_set_ellipsize ::
	Ptr (PangoLayoutPrim s) -> #{type PangoEllipsizeMode} -> IO ()

pangoLayoutSetIndent :: PrimMonad m => PangoLayoutPrim (PrimState m) -> #{type int} -> m ()
pangoLayoutSetIndent (PangoLayoutPrim fpl) idt = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> c_pango_layout_set_indent pl idt

foreign import ccall "pango_layout_set_indent" c_pango_layout_set_indent ::
	Ptr (PangoLayoutPrim s) -> #{type int} -> IO ()

{-
foreign import ccall "pango_layout_set_line_spacing" c_pango_layout_set_line_spacing ::
	Ptr PangoLayoutIo -> #{type float} -> IO ()

pangoLayoutSetLineSpacing :: PangoLayoutIo -> #{type float} -> IO ()
pangoLayoutSetLineSpacing (PangoLayoutIo fpl) fct = withForeignPtr fpl \pl ->
	c_pango_layout_set_line_spacing pl fct
	-}

pangoLayoutSetAlignment :: PrimMonad m => PangoLayoutPrim (PrimState m) -> PangoAlignment -> m ()
pangoLayoutSetAlignment (PangoLayoutPrim fpl) (PangoAlignment pa) = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> c_pango_layout_set_alignment pl pa

foreign import ccall "pango_layout_set_alignment" c_pango_layout_set_alignment ::
	Ptr (PangoLayoutPrim s) -> #{type PangoAlignment} -> IO ()

pangoLayoutSetTabs :: PrimMonad m => PangoLayoutPrim (PrimState m) -> PangoTabArray -> m ()
pangoLayoutSetTabs (PangoLayoutPrim fpl) (PangoTabArray fpta) = unsafeIOToPrim
	$ withForeignPtr fpl \pl ->
		withForeignPtr fpta \pta -> c_pango_layout_set_tabs pl pta

foreign import ccall "pango_layout_set_tabs" c_pango_layout_set_tabs ::
	Ptr (PangoLayoutPrim s) -> Ptr PangoTabArray -> IO ()

pangoLayoutSetSingleParagraphMode :: PrimMonad m => PangoLayoutPrim (PrimState m) -> Bool -> m ()
pangoLayoutSetSingleParagraphMode (PangoLayoutPrim fpl) spm = unsafeIOToPrim
	$ withForeignPtr fpl \pl ->
		c_pango_layout_set_single_paragraph_mode pl (boolToGboolean spm)

foreign import ccall "pango_layout_set_single_paragraph_mode"
	c_pango_layout_set_single_paragraph_mode ::
	Ptr (PangoLayoutPrim s) -> #{type gboolean} -> IO ()

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

foreign import ccall "pango_layout_get_lines_readonly" c_pango_layout_get_lines_readonly ::
	Ptr PangoLayout -> IO (Ptr (GSList PangoLayoutLine))

pangoLayoutGetLines :: PangoLayout -> [PangoLayoutLine]
pangoLayoutGetLines (PangoLayout fpl) = unsafePerformIO
	$ withForeignPtr fpl \pl ->
		mapM makePangoLayoutLine0 =<< g_slist_to_list =<< c_pango_layout_get_lines_readonly pl

foreign import ccall "pango_layout_get_iter" c_pango_layout_get_iter ::
	Ptr PangoLayout -> IO (Ptr (PangoLayoutIter s))

pangoLayoutGetIter :: PrimMonad m => PangoLayout -> m (PangoLayoutIter (PrimState m))
pangoLayoutGetIter (PangoLayout fpl) = unsafeIOToPrim
	$ makePangoLayoutIter =<< withForeignPtr fpl c_pango_layout_get_iter

foreign import ccall "pango_layout_iter_next_run" c_pango_layout_iter_next_run ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterNextRun :: PrimMonad m => PangoLayoutIter (PrimState m) -> m Bool
pangoLayoutIterNextRun (PangoLayoutIter fpli) = unsafeIOToPrim
	$ gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_run

foreign import ccall "pango_layout_iter_next_char" c_pango_layout_iter_next_char ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterNextChar :: PrimMonad m => PangoLayoutIter (PrimState m) -> m Bool
pangoLayoutIterNextChar (PangoLayoutIter fpli) = unsafeIOToPrim
	$ gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_char

foreign import ccall "pango_layout_iter_next_cluster" c_pango_layout_iter_next_cluster ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterNextCluster :: PrimMonad m => PangoLayoutIter (PrimState m) -> m Bool
pangoLayoutIterNextCluster (PangoLayoutIter fpli) = unsafeIOToPrim
	$ gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_cluster

foreign import ccall "pango_layout_iter_next_line" c_pango_layout_iter_next_line ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterNextLine :: PrimMonad m => PangoLayoutIter (PrimState m) -> m Bool
pangoLayoutIterNextLine (PangoLayoutIter fpli) = unsafeIOToPrim
	$ gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_line

foreign import ccall "pango_layout_iter_at_last_line" c_pango_layout_iter_at_last_line ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterAtLastLine :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m Bool
pangoLayoutIterAtLastLine (PangoLayoutIter fpli) = unsafeIOToPrim
	$ gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_at_last_line

foreign import ccall "pango_layout_iter_get_index" c_pango_layout_iter_get_index ::
	Ptr (PangoLayoutIter s) -> IO #type int

pangoLayoutIterGetIndex :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m #type int
pangoLayoutIterGetIndex (PangoLayoutIter fpli) = unsafeIOToPrim
	$ withForeignPtr fpli c_pango_layout_iter_get_index

foreign import ccall "pango_layout_iter_get_baseline" c_pango_layout_iter_get_baseline ::
	Ptr (PangoLayoutIter s) -> IO #type int

pangoLayoutIterGetBaseline :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m #type int
pangoLayoutIterGetBaseline (PangoLayoutIter fpli) = unsafeIOToPrim
	$ withForeignPtr fpli c_pango_layout_iter_get_baseline

foreign import ccall "pango_layout_iter_get_run_readonly" c_pango_layout_iter_get_run_readonly ::
	Ptr (PangoLayoutIter s) -> IO (Ptr PangoLayoutRun)

pangoLayoutIterGetRun :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m (Maybe PangoLayoutRun)
pangoLayoutIterGetRun (PangoLayoutIter fpli) = unsafeIOToPrim
	$ makePangoGlyphItemMaybe0 =<< withForeignPtr fpli c_pango_layout_iter_get_run_readonly

foreign import ccall "pango_layout_iter_get_line_readonly" c_pango_layout_iter_get_line_readonly ::
	Ptr (PangoLayoutIter s) -> IO (Ptr PangoLayoutLine)

pangoLayoutIterGetLine :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m PangoLayoutLine
pangoLayoutIterGetLine (PangoLayoutIter fpli) = unsafeIOToPrim
	$ makePangoLayoutLine0 =<< withForeignPtr fpli c_pango_layout_iter_get_line_readonly

foreign import ccall "pango_layout_iter_get_layout" c_pango_layout_iter_get_layout ::
	Ptr (PangoLayoutIter s) -> IO (Ptr PangoLayout)

pangoLayoutIterGetLayout :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m PangoLayout
pangoLayoutIterGetLayout (PangoLayoutIter fpli) = unsafeIOToPrim
	$ makePangoLayout0 =<< withForeignPtr fpli c_pango_layout_iter_get_layout

foreign import ccall "pango_layout_iter_get_char_extents" c_pango_layout_iter_get_char_extents ::
	Ptr (PangoLayoutIter s) -> Ptr PangoRectangle -> IO ()

pangoLayoutIterGetCharExtents :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m PangoRectangle
pangoLayoutIterGetCharExtents (PangoLayoutIter fpli) = unsafeIOToPrim
	$ withForeignPtr fpli \pli -> alloca \rct -> do
		c_pango_layout_iter_get_char_extents pli rct
		peek rct

foreign import ccall "pango_layout_iter_get_cluster_extents" c_pango_layout_iter_get_cluster_extents ::
	Ptr (PangoLayoutIter s) -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutIterGetClusterExtents :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m (PangoRectangle, PangoRectangle)
pangoLayoutIterGetClusterExtents (PangoLayoutIter fpli) = unsafeIOToPrim
	$ withForeignPtr fpli \pli -> alloca \irct -> alloca \lrct -> do
		c_pango_layout_iter_get_cluster_extents pli irct lrct
		(,) <$> peek irct <*> peek lrct

foreign import ccall "pango_layout_iter_get_run_extents" c_pango_layout_iter_get_run_extents ::
	Ptr (PangoLayoutIter s) -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutIterGetRunExtents :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m (PangoRectangle, PangoRectangle)
pangoLayoutIterGetRunExtents (PangoLayoutIter fpli) = unsafeIOToPrim
	$ withForeignPtr fpli \pli -> alloca \irct -> alloca \lrct -> do
		c_pango_layout_iter_get_run_extents pli irct lrct
		(,) <$> peek irct <*> peek lrct

foreign import ccall "pango_layout_iter_get_line_yrange" c_pango_layout_iter_get_line_yrange ::
	Ptr (PangoLayoutIter s) -> Ptr #{type int} -> Ptr #{type int} -> IO ()

pangoLayoutIterGetLineYrange :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m (#{type int}, #{type int})
pangoLayoutIterGetLineYrange (PangoLayoutIter fpli) = unsafeIOToPrim
	$ withForeignPtr fpli \pli -> alloca \y0 -> alloca \y1 -> do
		c_pango_layout_iter_get_line_yrange pli y0 y1
		(,) <$> peek y0 <*> peek y1

foreign import ccall "pango_layout_iter_get_line_extents" c_pango_layout_iter_get_line_extents ::
	Ptr (PangoLayoutIter s) -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutIterGetLineExtents :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m (PangoRectangle, PangoRectangle)
pangoLayoutIterGetLineExtents (PangoLayoutIter fpli) = unsafeIOToPrim
	$ withForeignPtr fpli \pli -> alloca \irct -> alloca \lrct -> do
		c_pango_layout_iter_get_line_extents pli irct lrct
		(,) <$> peek irct <*> peek lrct

foreign import ccall "pango_layout_iter_get_layout_extents" c_pango_layout_iter_get_layout_extents ::
	Ptr (PangoLayoutIter s) -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutIterGetLayoutExtents :: PrimMonad m =>
	PangoLayoutIter (PrimState m) -> m (PangoRectangle, PangoRectangle)
pangoLayoutIterGetLayoutExtents (PangoLayoutIter fpli) = unsafeIOToPrim
	$ withForeignPtr fpli \pli -> alloca \irct -> alloca \lrct -> do
		c_pango_layout_iter_get_layout_extents pli irct lrct
		(,) <$> peek irct <*> peek lrct

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

foreign import ccall "pango_layout_line_index_to_x" c_pango_layout_line_index_to_x ::
	Ptr PangoLayoutLine -> #{type int} -> #{type gboolean} -> Ptr #{type int} -> IO ()

pangoLayoutLineIndexToX :: PangoLayoutLine -> #{type int} -> Bool -> #type int
pangoLayoutLineIndexToX (PangoLayoutLine fpll) idx trl = unsafePerformIO
	$ withForeignPtr fpll \pll -> alloca \xpos -> do
		c_pango_layout_line_index_to_x pll idx (boolToGboolean trl) xpos
		peek xpos

foreign import ccall "pango_layout_line_x_to_index" c_pango_layout_line_x_to_index ::
	Ptr PangoLayoutLine -> #{type int} -> Ptr #{type int} -> Ptr #{type int} -> IO #type gboolean

pangoLayoutLineXToIndex :: PangoLayoutLine -> #{type int} -> (#{type int}, #{type int}, Bool)
pangoLayoutLineXToIndex (PangoLayoutLine fpll) xpos = unsafePerformIO
	$ withForeignPtr fpll \pll -> alloca \idx -> alloca \trl -> do
		isd <- c_pango_layout_line_x_to_index pll xpos idx trl
		(,,) <$> peek idx <*> peek trl <*> pure (gbooleanToBool isd)

foreign import ccall "pango_layout_line_get_x_ranges" c_pango_layout_line_get_x_ranges ::
	Ptr PangoLayoutLine -> #{type int} -> #{type int} -> Ptr (Ptr #{type int}) -> Ptr #{type int} -> IO ()

foreign import ccall "g_free" c_g_free :: Ptr a -> IO ()

pangoLayoutLineGetXRanges :: PangoLayoutLine -> #{type int} -> #{type int} -> [#type int]
pangoLayoutLineGetXRanges (PangoLayoutLine fpll) st ed = unsafePerformIO
	$ withForeignPtr fpll \pll -> alloca \prngs -> alloca \pn -> do
		c_pango_layout_line_get_x_ranges pll st ed prngs pn
		rngs <- peek prngs
		n <- peek pn
		peekArray (fromIntegral $ 2 * n) rngs <* c_g_free rngs

{-
foreign import ccall "pango_layout_line_get_height" c_pango_layout_line_get_height ::
	Ptr PangoLayoutLine -> Ptr #{type int} -> IO ()

pangoLayoutLineGetHeight :: PangoLayoutLine -> #{type int}
pangoLayoutLineGetHeight (PangoLayoutLine fpll) = unsafePerformIO
	$ withForeignPtr fpll \pll -> alloca \h -> do
		c_pango_layout_line_get_height pll h
		peek h
		-}
