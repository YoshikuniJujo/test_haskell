{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayout where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.String.Utf8
import Foreign.C.String.Tools
import Control.Monad.Primitive
import Data.Maybe
import Data.List
import Data.Bool
import Data.Word
import Data.Int
import Data.Char

import Data.Text.CString

import System.IO.Unsafe

import Graphics.Pango.Types
import Graphics.Pango.Values
import Graphics.Pango.Basic.Rendering
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.TextAttributes.Internal
import Graphics.Pango.Basic.LayoutObjects.PangoLayout.Template

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

import Graphics.Pango.PangoFixed
import Graphics.Pango.PangoRectangle

#include <pango/pango.h>
#include "pango_log_attr.h"

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

instance PangoLayoutSetting PangoTextAttrList where
	pangoLayoutSet = pangoLayoutSetTextAttributes
	pangoLayoutGet = pangoLayoutGetTextAttributes

pangoLayoutSetTextAttributes :: PangoLayout -> PangoTextAttrList -> IO ()
pangoLayoutSetTextAttributes
	l@(PangoLayout fl) (PangoTextAttrList (cs, ln) al) = do
	withForeignPtr fl \pl -> withForeignPtr cs \cs' ->  c_pango_layout_set_text pl cs' $ fromIntegral ln
	pangoLayoutSetAttributes l al

pangoLayoutGetTextAttributes :: PangoLayout -> IO PangoTextAttrList
pangoLayoutGetTextAttributes l@(PangoLayout fl) = PangoTextAttrList
	<$> (copyToForeignCStringLen =<< toCStringLen =<< withForeignPtr fl c_pango_layout_get_text)
	<*> pangoLayoutGetAttributes l

pangoLayoutSetAttributes :: PangoLayout -> PangoAttrList -> IO ()
pangoLayoutSetAttributes (PangoLayout fl) al = -- (PangoAttrList fal) =
	withForeignPtr fl \pl -> ($ c_pango_layout_set_attributes pl) case al of
		PangoAttrListNull -> ($ nullPtr)
		PangoAttrList fal -> withForeignPtr fal

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

instance PangoLayoutSetting PangoFontDescription where
	pangoLayoutSet = pangoLayoutSetFontDescription
	pangoLayoutGet = pangoLayoutGetFontDescription

pangoLayoutSetFontDescription :: PangoLayout -> PangoFontDescription -> IO ()
pangoLayoutSetFontDescription (PangoLayout fpl) fd =
	withForeignPtr fpl \pl -> ($ c_pango_layout_set_font_description pl) case fd of
		PangoFontDescriptionNull -> ($ nullPtr)
		PangoFontDescription ffd -> withForeignPtr ffd

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr PangoLayout -> Ptr PangoFontDescription -> IO ()

pangoLayoutGetFontDescription :: PangoLayout -> IO PangoFontDescription
pangoLayoutGetFontDescription (PangoLayout fpl) =
	mkPangoFontDescription =<< withForeignPtr fpl \ppl ->
		c_pango_font_description_copy
			=<< c_pango_layout_get_font_description ppl

foreign import ccall "pango_layout_get_font_description" c_pango_layout_get_font_description ::
	Ptr PangoLayout -> IO (Ptr PangoFontDescription)

foreign import ccall "pango_font_description_copy" c_pango_font_description_copy ::
	Ptr PangoFontDescription -> IO (Ptr PangoFontDescription)

data Width = WidthDefault | Width Double deriving Show

getWidth :: Width -> CInt
getWidth = \case WidthDefault -> - 1; Width w -> round $ w * #{const PANGO_SCALE}

width :: CInt -> Width
width = \case - 1 -> WidthDefault; w -> Width $ fromIntegral w / #{const PANGO_SCALE}

instance PangoLayoutSetting Width where
	pangoLayoutSet l = pangoLayoutSetWidth l . getWidth
	pangoLayoutGet l = width <$> pangoLayoutGetWidth l

data Height = HeightDefault | Height Double | LinesPerParagraph CInt
	deriving Show

toHeight :: CInt -> Height
toHeight n
	| n < 0 = LinesPerParagraph $ - n
	| otherwise = Height $ fromIntegral n / #{const PANGO_SCALE}

fromHeight :: Height -> CInt
fromHeight = \case
	HeightDefault -> - 1
	Height d -> round $ d * #{const PANGO_SCALE}
	LinesPerParagraph n -> - n

instance PangoLayoutSetting Height where
	pangoLayoutSet l = pangoLayoutSetHeight l . fromHeight
	pangoLayoutGet l = toHeight <$> pangoLayoutGetHeight l

pangoLayoutSetWidth, pangoLayoutSetHeight :: PangoLayout -> CInt -> IO ()
pangoLayoutSetWidth (PangoLayout fl) w =
	withForeignPtr fl \pl -> c_pango_layout_set_width pl w

pangoLayoutSetHeight (PangoLayout fl) h =
	withForeignPtr fl \pl -> c_pango_layout_set_height pl h

pangoLayoutGetWidth, pangoLayoutGetHeight :: PangoLayout -> IO CInt
pangoLayoutGetWidth (PangoLayout fl) =
	withForeignPtr fl c_pango_layout_get_width

pangoLayoutGetHeight (PangoLayout fl) =
	withForeignPtr fl c_pango_layout_get_height

foreign import ccall "pango_layout_set_width" c_pango_layout_set_width ::
	Ptr PangoLayout -> CInt -> IO ()

foreign import ccall "pango_layout_get_width" c_pango_layout_get_width ::
	Ptr PangoLayout -> IO CInt

foreign import ccall "pango_layout_set_height" c_pango_layout_set_height ::
	Ptr PangoLayout -> CInt -> IO ()

foreign import ccall "pango_layout_get_height" c_pango_layout_get_height ::
	Ptr PangoLayout -> IO CInt

mkMemberPangoWrapMode "PangoWrapWord" #{const PANGO_WRAP_WORD}
mkMemberPangoWrapMode "PangoWrapChar" #{const PANGO_WRAP_CHAR}
mkMemberPangoWrapMode "PangoWrapWordChar" #{const PANGO_WRAP_WORD_CHAR}

instance PangoLayoutSetting PangoWrapMode where
	pangoLayoutSet = pangoLayoutSetWrap
	pangoLayoutGet = pangoLayoutGetWrap

pangoLayoutSetWrap :: PangoLayout -> PangoWrapMode -> IO ()
pangoLayoutSetWrap (PangoLayout fl) (PangoWrapMode wm) =
	withForeignPtr fl \pl -> c_pango_layout_set_wrap pl wm

pangoLayoutGetWrap :: PangoLayout -> IO PangoWrapMode
pangoLayoutGetWrap (PangoLayout fl) = PangoWrapMode
	<$> withForeignPtr fl c_pango_layout_get_wrap

foreign import ccall "pango_layout_set_wrap" c_pango_layout_set_wrap ::
	Ptr PangoLayout -> #{type PangoWrapMode} -> IO ()

foreign import ccall "pango_layout_get_wrap" c_pango_layout_get_wrap ::
	Ptr PangoLayout -> IO #{type PangoWrapMode}

instance PangoLayoutSetting PangoEllipsizeMode where
	pangoLayoutSet = pangoLayoutSetEllipsize
	pangoLayoutGet = pangoLayoutGetEllipsize

pangoLayoutSetEllipsize :: PangoLayout -> PangoEllipsizeMode -> IO ()
pangoLayoutSetEllipsize (PangoLayout fpl) (PangoEllipsizeMode pem) =
	withForeignPtr fpl \pl -> c_pango_layout_set_ellipsize pl pem

pangoLayoutGetEllipsize :: PangoLayout -> IO PangoEllipsizeMode
pangoLayoutGetEllipsize (PangoLayout fl) = PangoEllipsizeMode
	<$> withForeignPtr fl c_pango_layout_get_ellipsize

foreign import ccall "pango_layout_set_ellipsize" c_pango_layout_set_ellipsize ::
	Ptr PangoLayout -> #{type PangoEllipsizeMode} -> IO ()

foreign import ccall "pango_layout_get_ellipsize" c_pango_layout_get_ellipsize ::
	Ptr PangoLayout -> IO #{type PangoEllipsizeMode}

newtype Indent = Indent { getIndent :: Double } deriving Show

instance PangoLayoutSetting Indent where
	pangoLayoutSet l =
		pangoLayoutSetIndent l . round . (* #{const PANGO_SCALE}) . getIndent
	pangoLayoutGet l =
		Indent . (/ #{const PANGO_SCALE}) . fromIntegral <$> pangoLayoutGetIndent l

pangoLayoutSetIndent :: PangoLayout -> CInt -> IO ()
pangoLayoutSetIndent (PangoLayout fl) idt =
	withForeignPtr fl \pl -> c_pango_layout_set_indent pl idt

pangoLayoutGetIndent :: PangoLayout -> IO CInt
pangoLayoutGetIndent (PangoLayout fl) =
	withForeignPtr fl c_pango_layout_get_indent

foreign import ccall "pango_layout_set_indent" c_pango_layout_set_indent ::
	Ptr PangoLayout -> CInt -> IO ()

foreign import ccall "pango_layout_get_indent" c_pango_layout_get_indent ::
	Ptr PangoLayout -> IO CInt

newtype Spacing = Spacing { getSpacing :: Double } deriving Show

instance PangoLayoutSetting Spacing where
	pangoLayoutSet l = pangoLayoutSetSpacing l
		. round . (* #{const PANGO_SCALE}) . getSpacing
	pangoLayoutGet l = Spacing . (/ #{const PANGO_SCALE})
		. fromIntegral <$> pangoLayoutGetSpacing l

pangoLayoutSetSpacing :: PangoLayout -> CInt -> IO ()
pangoLayoutSetSpacing (PangoLayout fl) sp =
	withForeignPtr fl \pl -> c_pango_layout_set_spacing pl sp

pangoLayoutGetSpacing :: PangoLayout -> IO CInt
pangoLayoutGetSpacing (PangoLayout fl) =
	withForeignPtr fl c_pango_layout_get_spacing

foreign import ccall "pango_layout_set_spacing" c_pango_layout_set_spacing ::
	Ptr PangoLayout -> CInt -> IO ()

foreign import ccall "pango_layout_get_spacing" c_pango_layout_get_spacing ::
	Ptr PangoLayout -> IO CInt

{-
foreign import ccall "pango_layout_set_line_spacing" c_pango_layout_set_line_spacing ::
	Ptr PangoLayoutIo -> #{type float} -> IO ()

pangoLayoutSetLineSpacing :: PangoLayoutIo -> #{type float} -> IO ()
pangoLayoutSetLineSpacing (PangoLayoutIo fpl) fct = withForeignPtr fpl \pl ->
	c_pango_layout_set_line_spacing pl fct
	-}

newtype Justify = Justify { getJustify :: Bool } deriving Show

instance PangoLayoutSetting Justify where
	pangoLayoutSet l = pangoLayoutSetJustify l . getJustify
	pangoLayoutGet l = Justify <$> pangoLayoutGetJustify l

pangoLayoutSetJustify :: PangoLayout -> Bool -> IO ()
pangoLayoutSetJustify (PangoLayout fl) b = withForeignPtr fl \pl ->
	c_pango_layout_set_justify pl $ bool #{const FALSE} #{const TRUE} b

pangoLayoutGetJustify :: PangoLayout -> IO Bool
pangoLayoutGetJustify (PangoLayout fl) =
	(<$> withForeignPtr fl c_pango_layout_get_justify) \case
		#{const FALSE} -> False
		#{const TRUE} -> True
		_ -> error "never occur"

foreign import ccall "pango_layout_set_justify" c_pango_layout_set_justify ::
	Ptr PangoLayout -> #{type gboolean} -> IO ()

foreign import ccall "pango_layout_get_justify" c_pango_layout_get_justify ::
	Ptr PangoLayout -> IO #{type gboolean}

newtype AutoDir = AutoDir { getAutoDir :: Bool } deriving Show

instance PangoLayoutSetting AutoDir where
	pangoLayoutSet l = pangoLayoutSetAutoDir l . getAutoDir
	pangoLayoutGet l = AutoDir <$> pangoLayoutGetAutoDir l

pangoLayoutSetAutoDir :: PangoLayout -> Bool -> IO ()
pangoLayoutSetAutoDir (PangoLayout fl) b = withForeignPtr fl \pl ->
	c_pango_layout_set_auto_dir pl $ bool #{const FALSE} #{const TRUE} b

pangoLayoutGetAutoDir :: PangoLayout -> IO Bool
pangoLayoutGetAutoDir (PangoLayout fl) =
	(<$> withForeignPtr fl c_pango_layout_get_auto_dir) \case
		#{const FALSE} -> False
		#{const TRUE} -> True
		_ -> error "never occur"

foreign import ccall "pango_layout_set_auto_dir" c_pango_layout_set_auto_dir ::
	Ptr PangoLayout -> #{type gboolean} -> IO ()

foreign import ccall "pango_layout_get_auto_dir" c_pango_layout_get_auto_dir ::
	Ptr PangoLayout -> IO #{type gboolean}

instance PangoLayoutSetting PangoAlignment where
	pangoLayoutSet = pangoLayoutSetAlignment
	pangoLayoutGet = pangoLayoutGetAlignment

pangoLayoutSetAlignment :: PangoLayout -> PangoAlignment -> IO ()
pangoLayoutSetAlignment (PangoLayout fpl) (PangoAlignment pa) =
	withForeignPtr fpl \pl -> c_pango_layout_set_alignment pl pa

pangoLayoutGetAlignment :: PangoLayout -> IO PangoAlignment
pangoLayoutGetAlignment (PangoLayout fl) =
	PangoAlignment <$> withForeignPtr fl c_pango_layout_get_alignment

foreign import ccall "pango_layout_set_alignment" c_pango_layout_set_alignment ::
	Ptr PangoLayout -> #{type PangoAlignment} -> IO ()

foreign import ccall "pango_layout_get_alignment" c_pango_layout_get_alignment ::
	Ptr PangoLayout -> IO #{type PangoAlignment}

instance PangoLayoutSetting PangoTabArray where
	pangoLayoutSet = pangoLayoutSetTabs
	pangoLayoutGet = pangoLayoutGetTabs

pangoLayoutSetTabs :: PangoLayout -> PangoTabArray -> IO ()
pangoLayoutSetTabs (PangoLayout fl) ta = unsafeIOToPrim
	$ withForeignPtr fl \pl -> ($ c_pango_layout_set_tabs pl) case ta of
		PangoTabArrayNull -> ($ nullPtr)
		PangoTabArray fta -> withForeignPtr fta

pangoLayoutGetTabs :: PangoLayout -> IO PangoTabArray
pangoLayoutGetTabs (PangoLayout fl) = unsafeIOToPrim
	$ makePangoTabArray =<< withForeignPtr fl c_pango_layout_get_tabs

foreign import ccall "pango_layout_set_tabs" c_pango_layout_set_tabs ::
	Ptr PangoLayout -> Ptr PangoTabArray -> IO ()

foreign import ccall "pango_layout_get_tabs" c_pango_layout_get_tabs ::
	Ptr PangoLayout -> IO (Ptr PangoTabArray)

newtype SingleParagraphMode =
	SingleParagraphMode { getSingleParagraphMode :: Bool } deriving Show

instance PangoLayoutSetting SingleParagraphMode where
	pangoLayoutSet l =
		pangoLayoutSetSingleParagraphMode l . getSingleParagraphMode
	pangoLayoutGet l =
		SingleParagraphMode <$> pangoLayoutGetSingleParagraphMode l

pangoLayoutSetSingleParagraphMode :: PangoLayout -> Bool -> IO ()
pangoLayoutSetSingleParagraphMode (PangoLayout fl) spm = unsafeIOToPrim
	$ withForeignPtr fl \pl ->
		c_pango_layout_set_single_paragraph_mode pl (boolToGboolean spm)

pangoLayoutGetSingleParagraphMode :: PangoLayout -> IO Bool
pangoLayoutGetSingleParagraphMode (PangoLayout fl) = unsafeIOToPrim
	$ (<$> withForeignPtr fl c_pango_layout_get_single_paragraph_mode) \case
		#{const FALSE} -> False; #{const TRUE} -> True
		_ -> error "never occur"

foreign import ccall "pango_layout_set_single_paragraph_mode"
	c_pango_layout_set_single_paragraph_mode ::
	Ptr PangoLayout -> #{type gboolean} -> IO ()

foreign import ccall "pango_layout_get_single_paragraph_mode"
	c_pango_layout_get_single_paragraph_mode ::
	Ptr PangoLayout -> IO #{type gboolean}

class PangoLayoutInfo i where pangoLayoutInfo :: PangoLayout -> IO i

newtype CharacterCount = CharacterCount CInt deriving Show

instance PangoLayoutInfo CharacterCount where
	pangoLayoutInfo = (CharacterCount <$>) . pangoLayoutGetCharacterCount

pangoLayoutGetCharacterCount :: PangoLayout -> IO CInt
pangoLayoutGetCharacterCount (PangoLayout fpl) =
	withForeignPtr fpl c_pango_layout_get_character_count

foreign import ccall "pango_layout_get_character_count"
	c_pango_layout_get_character_count :: Ptr PangoLayout -> IO CInt

newtype IsEllipsized = IsEllipsized Bool deriving Show

instance PangoLayoutInfo IsEllipsized where
	pangoLayoutInfo = (IsEllipsized <$>) . pangoLayoutIsEllipsized

pangoLayoutIsEllipsized :: PangoLayout -> IO Bool
pangoLayoutIsEllipsized (PangoLayout fl) =
	(<$> withForeignPtr fl c_pango_layout_is_ellipsized) \case
		#{const FALSE} -> False
		#{const TRUE} -> True
		_ -> error "never occur"

foreign import ccall "pango_layout_is_ellipsized"
	c_pango_layout_is_ellipsized :: Ptr PangoLayout -> IO #{type gboolean}

newtype IsWrapped = IsWrapped Bool deriving Show

instance PangoLayoutInfo IsWrapped where
	pangoLayoutInfo = (IsWrapped <$>) . pangoLayoutIsWrapped

pangoLayoutIsWrapped :: PangoLayout -> IO Bool
pangoLayoutIsWrapped (PangoLayout fl) =
	(<$> withForeignPtr fl c_pango_layout_is_wrapped) \case
		#{const FALSE} -> False
		#{const TRUE} -> True
		_ -> error "never occur"

foreign import ccall "pango_layout_is_wrapped" c_pango_layout_is_wrapped ::
	Ptr PangoLayout -> IO #{type gboolean}

newtype UnknownGlyphsCount = UnknownGlyphsCount CInt deriving Show

instance PangoLayoutInfo UnknownGlyphsCount where
	pangoLayoutInfo = (UnknownGlyphsCount <$>) . pangoLayoutGetUnknownGlyphsCount

pangoLayoutGetUnknownGlyphsCount :: PangoLayout -> IO CInt
pangoLayoutGetUnknownGlyphsCount (PangoLayout fpl) =
	withForeignPtr fpl c_pango_layout_get_unknown_glyphs_count

foreign import ccall "pango_layout_get_unknown_glyphs_count"
	c_pango_layout_get_unknown_glyphs_count :: Ptr PangoLayout -> IO CInt

data PangoLogAttr = PangoLogAttr_ (ForeignPtr PangoLogAttr) Int deriving Show
data PangoLogAttrStruct

pattern PangoLogAttr ::
	Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool ->
	Bool -> Bool -> Bool -> Bool -> PangoLogAttr
pattern PangoLogAttr { 
	pangoLogAttrIsLineBreak,
	pangoLogAttrIsMandatoryBreak,
	pangoLogAttrIsCharBreak,
	pangoLogAttrIsWhite,
	pangoLogAttrIsCursorPosition,
	pangoLogAttrIsWordStart,
	pangoLogAttrIsWordEnd,
	pangoLogAttrIsSentenceBoundary,
	pangoLogAttrIsSentenceStart,
	pangoLogAttrIsSentenceEnd,
	pangoLogAttrBackspaceDeleteCharacter,
	pangoLogAttrIsExpandableSpace,
	pangoLogAttrIsWordBoundary } <- (pangoLogAttrExpand -> PangoLogAttrExpand
		pangoLogAttrIsLineBreak
		pangoLogAttrIsMandatoryBreak
		pangoLogAttrIsCharBreak
		pangoLogAttrIsWhite
		pangoLogAttrIsCursorPosition
		pangoLogAttrIsWordStart
		pangoLogAttrIsWordEnd
		pangoLogAttrIsSentenceBoundary
		pangoLogAttrIsSentenceStart
		pangoLogAttrIsSentenceEnd
		pangoLogAttrBackspaceDeleteCharacter
		pangoLogAttrIsExpandableSpace
		pangoLogAttrIsWordBoundary)

data PangoLogAttrExpand = PangoLogAttrExpand {
	pangoLogAttrExpandIsLineBreak :: Bool,
	pangoLogAttrExpandIsMandatoryBreak :: Bool,
	pangoLogAttrExpandIsCharBreak :: Bool,
	pangoLogAttrExpandIsWhite :: Bool,
	pangoLogAttrExpandIsCursorPosition :: Bool,
	pangoLogAttrExpandIsWordStart :: Bool,
	pangoLogAttrExpandIsWordEnd :: Bool,
	pangoLogAttrExpandIsSentenceBoundary :: Bool,
	pangoLogAttrExpandIsSentenceStart :: Bool,
	pangoLogAttrExpandIsSentenceEnd :: Bool,
	pangoLogAttrExpandBackspaceDeletesCharacter :: Bool,
	pangoLogAttrExpandIsExpandableSpace :: Bool,
	pangoLogAttrExpandIsWordBoundary :: Bool
	} deriving Show

pangoLogAttrExpand :: PangoLogAttr -> PangoLogAttrExpand
pangoLogAttrExpand (PangoLogAttr_ fb i) = unsafePerformIO
	$ withForeignPtr fb \pb_ -> let pb = pb_ `plusPtr` (i * #{size PangoLogAttr}) in
		allocaBytes #{size PangoLogAttrStr} \ps -> do
			c_pango_log_attr_to_struct pb ps
			PangoLogAttrExpand
				<$> #{peek PangoLogAttrStr, is_line_break} ps
				<*> #{peek PangoLogAttrStr, is_mandatory_break} ps
				<*> #{peek PangoLogAttrStr, is_char_break} ps
				<*> #{peek PangoLogAttrStr, is_white} ps
				<*> #{peek PangoLogAttrStr, is_cursor_position} ps
				<*> #{peek PangoLogAttrStr, is_word_start} ps
				<*> #{peek PangoLogAttrStr, is_word_end} ps
				<*> #{peek PangoLogAttrStr, is_sentence_boundary} ps
				<*> #{peek PangoLogAttrStr, is_sentence_start} ps
				<*> #{peek PangoLogAttrStr, is_sentence_end} ps
				<*> #{peek PangoLogAttrStr, backspace_deletes_character} ps
				<*> #{peek PangoLogAttrStr, is_expandable_space} ps
				<*> #{peek PangoLogAttrStr, is_word_boundary} ps

foreign import ccall "pango_log_attr_to_struct" c_pango_log_attr_to_struct ::
	Ptr PangoLogAttr -> Ptr PangoLogAttrStruct -> IO ()

instance PangoLayoutInfo PangoLogAttrs where
	pangoLayoutInfo = pangoLayoutGetLogAttrs

data PangoLogAttrs = PangoLogAttrs (ForeignPtr PangoLogAttr) CInt deriving Show

mkPangoLogAttrs :: Ptr PangoLogAttr -> CInt -> IO PangoLogAttrs
mkPangoLogAttrs p n =
	(`PangoLogAttrs` n) <$> newForeignPtr p (c_g_free_pango_log_attr p)

foreign import ccall "g_free" c_g_free_pango_log_attr ::
	Ptr PangoLogAttr -> IO ()

pangoLayoutGetLogAttrs :: PangoLayout -> IO PangoLogAttrs
pangoLayoutGetLogAttrs (PangoLayout fl) =
	withForeignPtr fl \pl -> alloca \plas -> alloca \pn -> do
		c_pango_layout_get_log_attrs pl plas pn
		uncurry mkPangoLogAttrs =<< (,) <$> peek plas <*> peek pn

foreign import ccall "pango_layout_get_log_attrs" c_pango_layout_get_log_attrs ::
	Ptr PangoLayout -> Ptr (Ptr PangoLogAttr) -> Ptr CInt -> IO ()

pangoLogAttrsGetSize :: PangoLogAttrs -> Int
pangoLogAttrsGetSize (PangoLogAttrs _ sz) = fromIntegral sz

pangoLogAttrsGetLogAttr :: PangoLogAttrs -> Int -> Maybe PangoLogAttr
pangoLogAttrsGetLogAttr (PangoLogAttrs fla sz) i
	| 0 <= i && i < fromIntegral sz = Just $ PangoLogAttr_ fla i
	| otherwise = Nothing

data Extents = Extents {
	extentsInkRect :: PangoRectangle,
	extentsLogicalRect :: PangoRectangle } deriving Show

instance PangoLayoutInfo Extents where
	pangoLayoutInfo = (uncurry Extents <$>) . pangoLayoutGetExtents

pangoLayoutGetExtents :: PangoLayout -> IO (PangoRectangle, PangoRectangle)
pangoLayoutGetExtents (PangoLayout fpl) = withForeignPtr fpl \pl -> do
	irct <- mallocBytes #{size PangoRectangle}
	lrct <- mallocBytes #{size PangoRectangle}
	c_pango_layout_get_extents pl irct lrct
	(,)	<$> (PangoRectangle_ <$> newForeignPtr irct (free irct))
		<*> (PangoRectangle_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_get_extents" c_pango_layout_get_extents ::
	Ptr PangoLayout -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

data PixelExtents = PixelExtents {
	pixelExtentsInkRect :: PangoRectanglePixel,
	pixelExtentsLogicalRect :: PangoRectanglePixel } deriving Show

instance PangoLayoutInfo PixelExtents where
	pangoLayoutInfo = (uncurry PixelExtents <$>) . pangoLayoutGetPixelExtents

pangoLayoutGetPixelExtents :: PangoLayout -> IO (PangoRectanglePixel, PangoRectanglePixel)
pangoLayoutGetPixelExtents (PangoLayout fpl) =
	withForeignPtr fpl \pl -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_get_pixel_extents pl irct lrct
		(,)	<$> (PangoRectanglePixel_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectanglePixel_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_get_pixel_extents" c_pango_layout_get_pixel_extents ::
	Ptr PangoLayout -> Ptr PangoRectanglePixel -> Ptr PangoRectanglePixel -> IO ()

data LayoutSize = LayoutSize {
	layoutSizeWidth :: PangoFixed, layoutSizeHeight :: PangoFixed }
	deriving Show

instance PangoLayoutInfo LayoutSize where
	pangoLayoutInfo = (uncurry LayoutSize <$>) . pangoLayoutGetSize

pangoLayoutGetSize :: PangoLayout -> IO (PangoFixed, PangoFixed)
pangoLayoutGetSize (PangoLayout fpl) =
	withForeignPtr fpl \pl -> alloca \pw -> alloca \ph -> do
		c_pango_layout_get_size pl pw ph
		(\w h -> (toPangoFixed w, toPangoFixed h)) <$> peek pw <*> peek ph

foreign import ccall "pango_layout_get_size" c_pango_layout_get_size ::
	Ptr PangoLayout -> Ptr CInt -> Ptr CInt -> IO ()

data LayoutPixelSize = LayoutPixelSize {
	layoutPixelSizeWidth :: CInt, layoutPixelSizeHeight :: CInt }
	deriving Show

instance PangoLayoutInfo LayoutPixelSize where
	pangoLayoutInfo = (uncurry LayoutPixelSize <$>) . pangoLayoutGetPixelSize

pangoLayoutGetPixelSize :: PangoLayout -> IO (CInt, CInt)
pangoLayoutGetPixelSize (PangoLayout fpl) =
	withForeignPtr fpl \pl -> alloca \w -> alloca \h -> do
		c_pango_layout_get_pixel_size pl w h
		(,) <$> peek w <*> peek h

foreign import ccall "pango_layout_get_pixel_size" c_pango_layout_get_pixel_size ::
	Ptr PangoLayout -> Ptr CInt -> Ptr CInt -> IO ()

newtype Baseline = Baseline CInt deriving Show

instance PangoLayoutInfo Baseline where
	pangoLayoutInfo = (Baseline <$>) . pangoLayoutGetBaseline

pangoLayoutGetBaseline :: PangoLayout -> IO CInt
pangoLayoutGetBaseline (PangoLayout fpl) =
	withForeignPtr fpl c_pango_layout_get_baseline

foreign import ccall "pango_layout_get_baseline" c_pango_layout_get_baseline ::
	Ptr PangoLayout -> IO CInt

newtype LineCount = LineCount CInt deriving Show

instance PangoLayoutInfo LineCount where
	pangoLayoutInfo = (LineCount <$>) . pangoLayoutGetLineCount

pangoLayoutGetLineCount :: PangoLayout -> IO CInt
pangoLayoutGetLineCount (PangoLayout fpl) =
	withForeignPtr fpl c_pango_layout_get_line_count

foreign import ccall "pango_layout_get_line_count" c_pango_layout_get_line_count ::
	Ptr PangoLayout -> IO CInt

pangoLayoutIndexToPos :: PangoLayout -> Int -> IO (Maybe PangoRectangle)
pangoLayoutIndexToPos (PangoLayout fl) idx = withForeignPtr fl \pl -> do
	pos <- mallocBytes #{size PangoRectangle}
	t <- c_pango_layout_get_text pl
	is <- byteIndices =<< toCStringLen t
	case is `maybeIndex` idx of
		Nothing -> pure Nothing
		Just i -> do
			c_pango_layout_index_to_pos pl (fromIntegral i) pos
			Just . PangoRectangle_ <$> newForeignPtr pos (free pos)

maybeIndex :: [a] -> Int -> Maybe a
maybeIndex _ i | i < 0 = Nothing
maybeIndex [] _ = Nothing
maybeIndex (x : _) 0 = Just x
maybeIndex (_ : xs) i = maybeIndex xs (i - 1)

foreign import ccall "pango_layout_index_to_pos" c_pango_layout_index_to_pos ::
	Ptr PangoLayout -> CInt -> Ptr PangoRectangle -> IO ()

pangoLayoutIndexToLineX :: PangoLayout -> Int -> Bool -> IO (Maybe (CInt, PangoFixed))
pangoLayoutIndexToLineX (PangoLayout fpl) idx tr =
	withForeignPtr fpl \pl -> alloca \ln -> alloca \xpos -> do
		t <- c_pango_layout_get_text pl
		is <- byteIndices =<< toCStringLen t
		case is `maybeIndex` idx of
			Nothing -> pure Nothing
			Just i -> do
				c_pango_layout_index_to_line_x pl (fromIntegral i) (boolToGboolean tr) ln xpos
				Just <$> ((,) <$> peek ln <*> (toPangoFixed <$> peek xpos))

foreign import ccall "pango_layout_index_to_line_x"
	c_pango_layout_index_to_line_x ::
	Ptr PangoLayout -> CInt -> #{type gboolean} -> Ptr CInt -> Ptr CInt -> IO ()

pangoLayoutXyToIndex :: PangoLayout -> PangoFixed -> PangoFixed -> IO (Int, CInt, Bool)
pangoLayoutXyToIndex (PangoLayout fpl) x_ y_ =
	withForeignPtr fpl \pl -> alloca \idx -> alloca \tr -> do
		t <- c_pango_layout_get_text pl
		is <- byteIndices =<< toCStringLen t
		isd <- c_pango_layout_xy_to_index pl x y idx tr
		(,,) <$> (fromJust . (`elemIndex` is) . fromIntegral <$> peek idx) <*> peek tr <*> pure (gbooleanToBool isd)
	where
	[x, y] = fromPangoFixed <$> [x_, y_]

foreign import ccall "pango_layout_xy_to_index" c_pango_layout_xy_to_index ::
	Ptr PangoLayout -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO #type gboolean

pangoLayoutGetCursorPos :: PangoLayout -> Int -> IO (Maybe (PangoRectangle, PangoRectangle))
pangoLayoutGetCursorPos (PangoLayout fpl) idx = withForeignPtr fpl \pl -> do
	spos <- mallocBytes #{size PangoRectangle}
	wpos <- mallocBytes #{size PangoRectangle}
	t <- c_pango_layout_get_text pl
	is <- byteIndices =<< toCStringLen t
	case is `maybeIndex` idx of
		Nothing -> pure Nothing
		Just i -> do
			c_pango_layout_get_cursor_pos pl (fromIntegral i) spos wpos
			(Just <$>) $ (,)
				<$> (PangoRectangle_ <$> newForeignPtr spos (free spos))
				<*> (PangoRectangle_ <$> newForeignPtr wpos (free wpos))

foreign import ccall "pango_layout_get_cursor_pos" c_pango_layout_get_cursor_pos ::
	Ptr PangoLayout -> CInt -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

data MinMax a = Min | Jst a | Max deriving Show
data Dir = L | R deriving Show

pangoLayoutMoveCursorVisually ::
	PangoLayout -> Bool -> Int -> Bool -> Dir -> IO (Maybe (MinMax Int, CInt))
pangoLayoutMoveCursorVisually (PangoLayout fpl) str oidx otr dir =
	withForeignPtr fpl \pl -> alloca \nidx -> alloca \ntr -> do
		t <- c_pango_layout_get_text pl
		is <- byteIndices =<< toCStringLen t
		case is `maybeIndex` oidx of
			Nothing -> pure Nothing
			Just i -> do
				c_pango_layout_move_cursor_visually pl
					(boolToGboolean str) (fromIntegral i) (bool 0 1 otr)
					(case dir of L -> - 1; R -> 1) nidx ntr
				nidx' <- peek nidx
				let	mnidx = case nidx of
						_	| nidx' < 0 -> Min
							| nidx' < maxBound -> Jst . fromJust . (`elemIndex` is) $ fromIntegral nidx'
							| otherwise -> Max
				ntr' <- peek ntr
				pure . Just $ (mnidx, ntr')

foreign import ccall "pango_layout_move_cursor_visually" c_pango_layout_move_cursor_visually ::
	Ptr PangoLayout -> #{type gboolean} -> CInt -> CInt -> CInt ->
	Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall "pango_extents_to_pixels" c_pango_extents_to_pixels ::
	Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoExtentsToPixelsInclusive ::
	PrimMonad m => PangoRectanglePrim (PrimState m) -> m ()
pangoExtentsToPixelsInclusive (PangoRectanglePrim fr) = unsafeIOToPrim
	$ withForeignPtr fr (`c_pango_extents_to_pixels` nullPtr)

pangoExtentsToPixelsNearest ::
	PrimMonad m => PangoRectanglePrim (PrimState m) -> m ()
pangoExtentsToPixelsNearest (PangoRectanglePrim fr) = unsafeIOToPrim
	. withForeignPtr fr $ c_pango_extents_to_pixels nullPtr

foreign import ccall "pango_layout_get_line_readonly" c_pango_layout_get_line_readonly ::
	Ptr PangoLayout -> CInt -> IO (Ptr PangoLayoutLine)
