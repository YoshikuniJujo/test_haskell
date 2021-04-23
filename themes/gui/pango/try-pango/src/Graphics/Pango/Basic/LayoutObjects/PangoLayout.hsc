{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayout where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Primitive
import Data.Word
import Data.Int
import Data.Char

import Data.Text.CString

import Graphics.Pango.Types
import Graphics.Pango.Values
import Graphics.Pango.Basic.Rendering
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.TextAttributes

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

#include <pango/pango.h>

newtype PangoLayout = PangoLayout (ForeignPtr PangoLayout) deriving Show

mkPangoLayout :: Ptr PangoLayout -> IO PangoLayout
mkPangoLayout p = PangoLayout <$> newForeignPtr p (c_g_object_unref p)

pangoLayoutNew :: PangoContext -> IO PangoLayout
pangoLayoutNew (PangoContext fc) =
	mkPangoLayout =<< withForeignPtr fc c_pango_layout_new

foreign import ccall "pango_layout_new" c_pango_layout_new ::
	Ptr PangoContext -> IO (Ptr PangoLayout)

class PangoLayoutSetting s where
	pangoLayoutSet :: PangoLayout -> s -> IO ()
	pangoLayoutGet :: PangoLayout -> IO s

instance PangoLayoutSetting T.Text where
	pangoLayoutSet = pangoLayoutSetText
	pangoLayoutGet = pangoLayoutGetText

pangoLayoutSetText :: PangoLayout -> T.Text -> IO ()
pangoLayoutSetText (PangoLayout fpl) s =
	withForeignPtr fpl \pl -> T.withCStringLen s \(cs, n) ->
		c_pango_layout_set_text pl cs $ fromIntegral n

foreign import ccall "pango_layout_set_text" c_pango_layout_set_text ::
	Ptr PangoLayout -> CString -> CInt -> IO ()

pangoLayoutGetText :: PangoLayout -> IO T.Text
pangoLayoutGetText (PangoLayout fpl) =
	withForeignPtr fpl \pl -> peekCStringText =<< c_pango_layout_get_text pl

foreign import ccall "pango_layout_get_text" c_pango_layout_get_text ::
	Ptr PangoLayout -> IO CString

pangoLayoutGetCharacterCount :: PangoLayout -> IO CInt
pangoLayoutGetCharacterCount (PangoLayout fpl) =
	withForeignPtr fpl c_pango_layout_get_character_count

foreign import ccall "pango_layout_get_character_count"
	c_pango_layout_get_character_count :: Ptr PangoLayout -> IO CInt

pangoLayoutSetMarkup :: PangoLayout -> T.Text -> IO ()
pangoLayoutSetMarkup (PangoLayout fpl) mu =
	withForeignPtr fpl \ppl -> T.withCStringLen mu \(cs, cl) ->
		c_pango_layout_set_markup ppl cs $ fromIntegral cl

foreign import ccall "pango_layout_set_markup"
	c_pango_layout_set_markup :: Ptr PangoLayout -> CString -> CInt -> IO ()

pangoLayoutSetMarkupWithAccel :: PangoLayout -> T.Text -> Char -> IO Char
pangoLayoutSetMarkupWithAccel (PangoLayout fpl) mu am =
	withForeignPtr fpl \ppl ->
		T.withCStringLen mu \(cs, cl) -> alloca \pa -> do
			c_pango_layout_set_markup_with_accel ppl cs (fromIntegral cl) (fromIntegral $ ord am) pa
			chr . fromIntegral <$> peek pa

foreign import ccall "pango_layout_set_markup_with_accel"
	c_pango_layout_set_markup_with_accel ::
	Ptr PangoLayout -> CString -> CInt ->
	#{type gunichar} -> Ptr #{type gunichar} -> IO ()

instance PangoLayoutSetting PangoAttrList where
	pangoLayoutSet = pangoLayoutSetAttributes
	pangoLayoutGet = pangoLayoutGetAttributes

pangoLayoutSetAttributes :: PangoLayout -> PangoAttrList -> IO ()
pangoLayoutSetAttributes (PangoLayout fpl) (PangoAttrList fpal) =
	withForeignPtr fpl \ppl -> withForeignPtr fpal \ppal ->
		c_pango_layout_set_attributes ppl ppal

foreign import ccall "pango_layout_set_attributes"
	c_pango_layout_set_attributes ::
	Ptr PangoLayout -> Ptr PangoAttrList -> IO ()

pangoLayoutGetAttributes :: PangoLayout -> IO PangoAttrList
pangoLayoutGetAttributes (PangoLayout fpl) =
	mkPangoAttrList =<< do
		p <- withForeignPtr fpl c_pango_layout_get_attributes
		p <$ c_pango_attr_list_ref p

foreign import ccall "pango_layout_get_attributes"
	c_pango_layout_get_attributes ::
	Ptr PangoLayout -> IO (Ptr PangoAttrList)

foreign import ccall "pango_attr_list_ref" c_pango_attr_list_ref ::
	Ptr PangoAttrList -> IO (Ptr PangoAttrList)

pangoLayoutSetFontDescription :: PangoLayout -> PangoFontDescription -> IO ()
pangoLayoutSetFontDescription (PangoLayout fpl) (PangoFontDescription fpfd) =
	withForeignPtr fpl \pl -> withForeignPtr fpfd \pfd ->
		c_pango_layout_set_font_description pl pfd

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr PangoLayout -> Ptr PangoFontDescription -> IO ()

pangoLayoutSetWidth :: PangoLayout -> CInt -> IO ()
pangoLayoutSetWidth (PangoLayout fpl) w =
	withForeignPtr fpl \pl -> c_pango_layout_set_width pl w

foreign import ccall "pango_layout_set_width" c_pango_layout_set_width ::
	Ptr PangoLayout -> CInt -> IO ()

foreign import ccall "pango_layout_get_width" c_pango_layout_get_width ::
	Ptr PangoLayout -> IO CInt

pangoLayoutGetWidth :: PangoLayout -> IO CInt
pangoLayoutGetWidth (PangoLayout fpl) =
	withForeignPtr fpl c_pango_layout_get_width

pangoLayoutSetEllipsize :: PangoLayout -> PangoEllipsizeMode -> IO ()
pangoLayoutSetEllipsize (PangoLayout fpl) (PangoEllipsizeMode pem) =
	withForeignPtr fpl \pl -> c_pango_layout_set_ellipsize pl pem

foreign import ccall "pango_layout_set_ellipsize" c_pango_layout_set_ellipsize ::
	Ptr PangoLayout -> #{type PangoEllipsizeMode} -> IO ()

pangoLayoutSetIndent :: PangoLayout -> CInt -> IO ()
pangoLayoutSetIndent (PangoLayout fpl) idt = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> c_pango_layout_set_indent pl idt

foreign import ccall "pango_layout_set_indent" c_pango_layout_set_indent ::
	Ptr PangoLayout -> CInt -> IO ()

{-
foreign import ccall "pango_layout_set_line_spacing" c_pango_layout_set_line_spacing ::
	Ptr PangoLayoutIo -> #{type float} -> IO ()

pangoLayoutSetLineSpacing :: PangoLayoutIo -> #{type float} -> IO ()
pangoLayoutSetLineSpacing (PangoLayoutIo fpl) fct = withForeignPtr fpl \pl ->
	c_pango_layout_set_line_spacing pl fct
	-}

pangoLayoutSetAlignment :: PangoLayout -> PangoAlignment -> IO ()
pangoLayoutSetAlignment (PangoLayout fpl) (PangoAlignment pa) = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> c_pango_layout_set_alignment pl pa

foreign import ccall "pango_layout_set_alignment" c_pango_layout_set_alignment ::
	Ptr PangoLayout -> #{type PangoAlignment} -> IO ()

pangoLayoutSetTabs :: PangoLayout -> PangoTabArray -> IO ()
pangoLayoutSetTabs (PangoLayout fpl) (PangoTabArray fpta) = unsafeIOToPrim
	$ withForeignPtr fpl \pl ->
		withForeignPtr fpta \pta -> c_pango_layout_set_tabs pl pta

foreign import ccall "pango_layout_set_tabs" c_pango_layout_set_tabs ::
	Ptr PangoLayout -> Ptr PangoTabArray -> IO ()

pangoLayoutSetSingleParagraphMode :: PangoLayout -> Bool -> IO ()
pangoLayoutSetSingleParagraphMode (PangoLayout fpl) spm = unsafeIOToPrim
	$ withForeignPtr fpl \pl ->
		c_pango_layout_set_single_paragraph_mode pl (boolToGboolean spm)

foreign import ccall "pango_layout_set_single_paragraph_mode"
	c_pango_layout_set_single_paragraph_mode ::
	Ptr PangoLayout -> #{type gboolean} -> IO ()

foreign import ccall "pango_layout_get_unknown_glyphs_count"
	c_pango_layout_get_unknown_glyphs_count :: Ptr PangoLayout -> IO CInt

pangoLayoutGetUnknownGlyphsCount :: PangoLayout -> IO CInt
pangoLayoutGetUnknownGlyphsCount (PangoLayout fpl) =
	withForeignPtr fpl c_pango_layout_get_unknown_glyphs_count

foreign import ccall "pango_layout_index_to_pos" c_pango_layout_index_to_pos ::
	Ptr PangoLayout -> CInt -> Ptr PangoRectangle -> IO ()

pangoLayoutIndexToPos :: PangoLayout -> CInt -> IO PangoRectangle
pangoLayoutIndexToPos (PangoLayout fpl) idx =
	withForeignPtr fpl \pl -> alloca \pos -> do
		c_pango_layout_index_to_pos pl idx pos
		peek pos

foreign import ccall "pango_layout_index_to_line_x"
	c_pango_layout_index_to_line_x ::
	Ptr PangoLayout -> CInt -> #{type gboolean} -> Ptr CInt -> Ptr CInt -> IO ()

pangoLayoutIndexToLineX :: PangoLayout -> CInt -> Bool -> IO (CInt, CInt)
pangoLayoutIndexToLineX (PangoLayout fpl) idx tr =
	withForeignPtr fpl \pl -> alloca \ln -> alloca \xpos -> do
		c_pango_layout_index_to_line_x pl idx (boolToGboolean tr) ln xpos
		(,) <$> peek ln <*> peek xpos

foreign import ccall "pango_layout_xy_to_index" c_pango_layout_xy_to_index ::
	Ptr PangoLayout -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO #type gboolean

pangoLayoutXyToIndex :: PangoLayout -> CInt -> CInt -> IO (CInt, CInt, Bool)
pangoLayoutXyToIndex (PangoLayout fpl) x y =
	withForeignPtr fpl \pl -> alloca \idx -> alloca \tr -> do
		isd <- c_pango_layout_xy_to_index pl x y idx tr
		(,,) <$> peek idx <*> peek tr <*> pure (gbooleanToBool isd)

foreign import ccall "pango_layout_get_cursor_pos" c_pango_layout_get_cursor_pos ::
	Ptr PangoLayout -> CInt -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutGetCursorPos :: PangoLayout -> CInt -> IO (PangoRectangle, PangoRectangle)
pangoLayoutGetCursorPos (PangoLayout fpl) idx =
	withForeignPtr fpl \pl -> alloca \spos -> alloca \wpos -> do
		c_pango_layout_get_cursor_pos pl idx spos wpos
		(,) <$> peek spos <*> peek wpos

foreign import ccall "pango_layout_move_cursor_visually" c_pango_layout_move_cursor_visually ::
	Ptr PangoLayout -> #{type gboolean} -> CInt -> CInt -> CInt ->
	Ptr CInt -> Ptr CInt -> IO ()

pangoLayoutMoveCursorVisually ::
	PangoLayout -> Bool -> CInt -> CInt -> CInt -> IO (CInt, CInt)
pangoLayoutMoveCursorVisually (PangoLayout fpl) str oidx otr dir =
	withForeignPtr fpl \pl -> alloca \nidx -> alloca \ntr -> do
		c_pango_layout_move_cursor_visually pl (boolToGboolean str) oidx otr dir nidx ntr
		(,) <$> peek nidx <*> peek ntr

foreign import ccall "pango_layout_get_extents" c_pango_layout_get_extents ::
	Ptr PangoLayout -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutGetExtents :: PangoLayout -> IO (PangoRectangle, PangoRectangle)
pangoLayoutGetExtents (PangoLayout fpl) =
	withForeignPtr fpl \pl -> alloca \irct -> alloca \lrct -> do
		c_pango_layout_get_extents pl irct lrct
		(,) <$> peek irct <*> peek lrct

foreign import ccall "pango_layout_get_pixel_extents" c_pango_layout_get_pixel_extents ::
	Ptr PangoLayout -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutGetPixelExtents :: PangoLayout -> IO (PangoRectangle, PangoRectangle)
pangoLayoutGetPixelExtents (PangoLayout fpl) =
	withForeignPtr fpl \pl -> alloca \irct -> alloca \lrct -> do
		c_pango_layout_get_pixel_extents pl irct lrct
		(,) <$> peek irct <*> peek lrct

foreign import ccall "pango_extents_to_pixels" c_pango_extents_to_pixels ::
	Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoExtentsToPixelsInclusive :: PangoRectangle -> IO PangoRectangle
pangoExtentsToPixelsInclusive src =
	alloca \dst -> do
		poke dst src
		c_pango_extents_to_pixels dst nullPtr
		peek dst

pangoExtentsToPixelsNearest :: PangoRectangle -> IO PangoRectangle
pangoExtentsToPixelsNearest src =
	alloca \dst -> do
		poke dst src
		c_pango_extents_to_pixels nullPtr dst
		peek dst

foreign import ccall "pango_layout_get_size" c_pango_layout_get_size ::
	Ptr PangoLayout -> Ptr CInt -> Ptr CInt -> IO ()

pangoLayoutGetSize :: PangoLayout -> IO (CInt, CInt)
pangoLayoutGetSize (PangoLayout fpl) =
	withForeignPtr fpl \pl -> alloca \w -> alloca \h -> do
		c_pango_layout_get_size pl w h
		(,) <$> peek w <*> peek h

foreign import ccall "pango_layout_get_pixel_size" c_pango_layout_get_pixel_size ::
	Ptr PangoLayout -> Ptr CInt -> Ptr CInt -> IO ()

pangoLayoutGetPixelSize :: PangoLayout -> IO (CInt, CInt)
pangoLayoutGetPixelSize (PangoLayout fpl) =
	withForeignPtr fpl \pl -> alloca \w -> alloca \h -> do
		c_pango_layout_get_pixel_size pl w h
		(,) <$> peek w <*> peek h

foreign import ccall "pango_layout_get_baseline" c_pango_layout_get_baseline ::
	Ptr PangoLayout -> IO CInt

pangoLayoutGetBaseline :: PangoLayout -> IO CInt
pangoLayoutGetBaseline (PangoLayout fpl) =
	withForeignPtr fpl c_pango_layout_get_baseline

foreign import ccall "pango_layout_get_line_count" c_pango_layout_get_line_count ::
	Ptr PangoLayout -> IO CInt

pangoLayoutGetLineCount :: PangoLayout -> IO CInt
pangoLayoutGetLineCount (PangoLayout fpl) =
	withForeignPtr fpl c_pango_layout_get_line_count

foreign import ccall "pango_layout_get_line_readonly" c_pango_layout_get_line_readonly ::
	Ptr PangoLayout -> CInt -> IO (Ptr PangoLayoutLine)
