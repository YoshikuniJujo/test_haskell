{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayout.Internal (
	-- * TYPE
	PangoLayout(..),
	PangoLayoutPrim(..), PangoLayoutST, PangoLayoutIO, mkPangoLayoutPrim,
	pangoLayoutFreeze, pangoLayoutThaw, pangoLayoutCopy,

	-- * CLASS
	-- ** PangoLayoutSetting
	PangoLayoutSetting, pangoLayoutSet, pangoLayoutGet,

	-- ** PangoLayoutInfo
	PangoLayoutInfo, pangoLayoutInfo,

	-- * FUNCTION
	pangoLayoutNew, pangoLayoutContextChanged,
	pangoLayoutSetMarkup, pangoLayoutSetMarkupWithAccel,

	pangoLayoutIndexToPos,
	pangoLayoutIndexToLineX,
	pangoLayoutXyToIndex,
	pangoLayoutGetCursorPos,
	pangoLayoutMoveCursorVisually, Dir(..),

	-- * SETTING

	-- ** Width and Height
	Width(..), Height(..),

	-- ** PangoWrapMode
	PangoWrapMode(..),
	pattern PangoWrapWord, pattern PangoWrapChar, pattern PangoWrapWordChar,

	-- ** PangoEllipsizeMode
	PangoEllipsizeMode(..),
	pattern PangoEllipsizeNone, pattern PangoEllipsizeStart,
	pattern PangoEllipsizeMiddle, pattern PangoEllipsizeEnd,

	-- ** Indent, Spacing, LineSpacing, Justify and AutoDir
	Indent(..), Spacing(..), LineSpacing(..), Justify(..), AutoDir(..),

	-- ** PangoAlignment
	PangoAlignment(..),
	pattern PangoAlignLeft, pattern PangoAlignCenter,
	pattern PangoAlignRight,

	-- ** SingleParagraphMode
	SingleParagraphMode(..),

	-- * INFO

	-- ** CharacterCount, IsWrapped, IsEllipsized and UnknownGlyphCount
	CharacterCount(..), IsWrapped(..), IsEllipsized(..), UnknownGlyphsCount(..),

	-- ** PangoLogAttrs
	PangoLogAttrs, pangoLogAttrsGetLogAttr, pangoLogAttrsGetSize,
	PangoLogAttr,
	pattern PangoLogAttr,
	pangoLogAttrIsLineBreak, pangoLogAttrIsMandatoryBreak,
	pangoLogAttrIsCharBreak, pangoLogAttrIsWhite,
	pangoLogAttrIsCursorPosition,
	pangoLogAttrIsWordStart, pangoLogAttrIsWordEnd,
	pangoLogAttrIsSentenceBoundary,
	pangoLogAttrIsSentenceStart, pangoLogAttrIsSentenceEnd,
	pangoLogAttrBackspaceDeleteCharacter, pangoLogAttrIsExpandableSpace,
	pangoLogAttrIsWordBoundary,

	-- ** LayoutSize, LayoutPixelSize, Baseline and LineCount
	LayoutSize(..), LayoutPixelSize(..), Baseline(..), LineCount(..),

	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.String.Utf8
import Foreign.C.String.ForeignCStringLen
import Foreign.C.String.Misc
import Foreign.C.Enum
import Foreign.C.Struct
import Control.Monad.Primitive
import Data.Maybe
import Data.List
import Data.Bool
import Data.Word
import Data.Int
import Data.Char
import Data.Text.CString
import System.IO.Unsafe
import System.Glib.Bool
import System.Glib.GObject

import Graphics.Pango.Basic.GlyphStorage.Internal
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.TextAttributes.Internal
import Graphics.Pango.LowLevel.Contexts.Internal
import Graphics.Pango.LowLevel.TabStops.Internal

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

#include <pango/pango.h>
#include "pango_log_attr.h"

newtype PangoLayout = PangoLayout_ (ForeignPtr PangoLayout) deriving Show

foreign import ccall "pango_layout_copy" c_pango_layout_copy ::
	Ptr PangoLayout -> IO (Ptr PangoLayout)

foreign import ccall "g_object_unref" c_pango_layout_free ::
	Ptr PangoLayout -> IO ()

structPrim "PangoLayout" 'c_pango_layout_copy 'c_pango_layout_free [''Show]

mkPangoLayoutPrim :: Ptr PangoLayout -> IO (PangoLayoutPrim s)
mkPangoLayoutPrim p = PangoLayoutPrim <$> newForeignPtr p (c_g_object_unref p)

pangoLayoutNew :: PrimMonad m => PangoContext -> m (PangoLayoutPrim (PrimState m))
pangoLayoutNew (PangoContext fc) = unsafeIOToPrim
	$ mkPangoLayoutPrim =<< withForeignPtr fc c_pango_layout_new

foreign import ccall "pango_layout_new" c_pango_layout_new ::
	Ptr PangoContext -> IO (Ptr PangoLayout)

pangoLayoutContextChanged ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> m ()
pangoLayoutContextChanged (PangoLayoutPrim fl) = unsafeIOToPrim
	$ withForeignPtr fl c_pango_layout_context_changed

foreign import ccall "pango_layout_context_changed"
	c_pango_layout_context_changed ::
	Ptr PangoLayout -> IO ()

class PangoLayoutSetting s where
	pangoLayoutSet ::
		PrimMonad m => PangoLayoutPrim (PrimState m) -> s -> m ()
	pangoLayoutGet :: PangoLayout -> s

instance PangoLayoutSetting T.Text where
	pangoLayoutSet = (unsafeIOToPrim .) . pangoLayoutSetText
	pangoLayoutGet = pangoLayoutGetText

pangoLayoutSetText :: PangoLayoutPrim s -> T.Text -> IO ()
pangoLayoutSetText (PangoLayoutPrim fpl) s =
	withForeignPtr fpl \pl -> T.withCStringLen s \(cs, n) -> do
		cs' <- copyCString cs n
		addForeignPtrFinalizer fpl $ free cs'
		c_pango_layout_set_text pl cs' $ fromIntegral n

foreign import ccall "pango_layout_set_text" c_pango_layout_set_text ::
	Ptr PangoLayout -> CString -> CInt -> IO ()

pangoLayoutGetText :: PangoLayout -> T.Text
pangoLayoutGetText (PangoLayout_ fpl) = unsafePerformIO
	$ withForeignPtr fpl \pl -> peekCStringText =<< c_pango_layout_get_text pl

foreign import ccall "pango_layout_get_text" c_pango_layout_get_text ::
	Ptr PangoLayout -> IO CString

pangoLayoutSetMarkup ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> T.Text -> m ()
pangoLayoutSetMarkup (PangoLayoutPrim fpl) mu = unsafeIOToPrim
	$ withForeignPtr fpl \ppl -> T.withCStringLen mu \(cs, cl) ->
		c_pango_layout_set_markup ppl cs $ fromIntegral cl

foreign import ccall "pango_layout_set_markup"
	c_pango_layout_set_markup :: Ptr PangoLayout -> CString -> CInt -> IO ()

pangoLayoutSetMarkupWithAccel ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> T.Text -> Char -> m Char
pangoLayoutSetMarkupWithAccel (PangoLayoutPrim fpl) mu am = unsafeIOToPrim
	$ withForeignPtr fpl \ppl ->
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

pangoLayoutSetTextAttributes :: PrimMonad m =>
	PangoLayoutPrim (PrimState m) -> PangoTextAttrList -> m ()
pangoLayoutSetTextAttributes
	l@(PangoLayoutPrim fl) (PangoTextAttrList (cs, ln) al) = unsafeIOToPrim do
	withForeignPtr fl \pl -> withForeignPtr cs \cs' ->  c_pango_layout_set_text pl cs' $ fromIntegral ln
	pangoLayoutSetAttributes l al

pangoLayoutGetTextAttributes :: PangoLayout -> PangoTextAttrList
pangoLayoutGetTextAttributes l@(PangoLayout_ fl) = unsafePerformIO $ PangoTextAttrList
	<$> (copyToForeignCStringLen =<< toCStringLen =<< withForeignPtr fl c_pango_layout_get_text)
	<*> pangoLayoutGetAttributes l

pangoLayoutSetAttributes :: PangoLayoutPrim s -> PangoAttrList -> IO ()
pangoLayoutSetAttributes (PangoLayoutPrim fl) al =
	withForeignPtr fl \pl -> ($ c_pango_layout_set_attributes pl) case al of
		PangoAttrListNull -> ($ nullPtr)
		PangoAttrList fal -> withForeignPtr fal

foreign import ccall "pango_layout_set_attributes"
	c_pango_layout_set_attributes ::
	Ptr PangoLayout -> Ptr PangoAttrList -> IO ()

pangoLayoutGetAttributes :: PangoLayout -> IO PangoAttrList
pangoLayoutGetAttributes (PangoLayout_ fpl) =
	mkPangoAttrList =<< do
		p <- withForeignPtr fpl c_pango_layout_get_attributes
		p <$ c_pango_attr_list_ref p

foreign import ccall "pango_layout_get_attributes"
	c_pango_layout_get_attributes ::
	Ptr PangoLayout -> IO (Ptr PangoAttrList)

foreign import ccall "pango_attr_list_ref" c_pango_attr_list_ref ::
	Ptr PangoAttrList -> IO (Ptr PangoAttrList)

instance PangoLayoutSetting PangoFontDescriptionNullable where
	pangoLayoutSet = pangoLayoutSetFontDescription
	pangoLayoutGet = pangoLayoutGetFontDescription

pangoLayoutSetFontDescription :: PrimMonad m =>
	PangoLayoutPrim (PrimState m) -> PangoFontDescriptionNullable -> m ()
pangoLayoutSetFontDescription (PangoLayoutPrim fpl) fd = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> case fd of
		PangoFontDescriptionNull -> c_pango_layout_set_font_description pl nullPtr
		PangoFontDescriptionNotNull ffd -> do
			addForeignPtrFinalizer fpl $ touchForeignPtr ffd
			withForeignPtr ffd $ c_pango_layout_set_font_description pl

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr PangoLayout -> Ptr PangoFontDescription -> IO ()

pangoLayoutGetFontDescription :: PangoLayout -> PangoFontDescriptionNullable
pangoLayoutGetFontDescription (PangoLayout_ fpl) = unsafePerformIO
	$ mkPangoFontDescriptionNullable =<< withForeignPtr fpl \ppl ->
		c_pango_font_description_copy
			=<< c_pango_layout_get_font_description ppl

foreign import ccall "pango_layout_get_font_description" c_pango_layout_get_font_description ::
	Ptr PangoLayout -> IO (Ptr PangoFontDescription)

foreign import ccall "pango_font_description_copy" c_pango_font_description_copy ::
	Ptr PangoFontDescription -> IO (Ptr PangoFontDescription)

data Width = WidthDefault | Width PangoFixed deriving Show

getWidth :: Width -> CInt
getWidth = \case WidthDefault -> - 1; Width w -> toCInt w

width :: CInt -> Width
width = \case - 1 -> WidthDefault; w -> Width $ fromCInt w

instance PangoLayoutSetting Width where
	pangoLayoutSet l = pangoLayoutSetWidth l . getWidth
	pangoLayoutGet l = width $ pangoLayoutGetWidth l

data Height = HeightDefault | Height PangoFixed | LinesPerParagraph CInt
	deriving Show

toHeight :: CInt -> Height
toHeight n
	| n < 0 = LinesPerParagraph $ - n
	| otherwise = Height $ fromCInt n

fromHeight :: Height -> CInt
fromHeight = \case
	HeightDefault -> - 1
	Height d -> toCInt d
	LinesPerParagraph n -> - n

instance PangoLayoutSetting Height where
	pangoLayoutSet l = pangoLayoutSetHeight l . fromHeight
	pangoLayoutGet l = toHeight $ pangoLayoutGetHeight l

pangoLayoutSetWidth, pangoLayoutSetHeight ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> CInt -> m ()
pangoLayoutSetWidth (PangoLayoutPrim fl) w = unsafeIOToPrim
	$ withForeignPtr fl \pl -> c_pango_layout_set_width pl w

pangoLayoutSetHeight (PangoLayoutPrim fl) h = unsafeIOToPrim
	$ withForeignPtr fl \pl -> c_pango_layout_set_height pl h

pangoLayoutGetWidth, pangoLayoutGetHeight :: PangoLayout -> CInt
pangoLayoutGetWidth (PangoLayout_ fl) = unsafePerformIO
	$ withForeignPtr fl c_pango_layout_get_width

pangoLayoutGetHeight (PangoLayout_ fl) = unsafePerformIO
	$ withForeignPtr fl c_pango_layout_get_height

foreign import ccall "pango_layout_set_width" c_pango_layout_set_width ::
	Ptr PangoLayout -> CInt -> IO ()

foreign import ccall "pango_layout_get_width" c_pango_layout_get_width ::
	Ptr PangoLayout -> IO CInt

foreign import ccall "pango_layout_set_height" c_pango_layout_set_height ::
	Ptr PangoLayout -> CInt -> IO ()

foreign import ccall "pango_layout_get_height" c_pango_layout_get_height ::
	Ptr PangoLayout -> IO CInt

enum "PangoWrapMode" ''#{type PangoWrapMode} [''Show] [
	("PangoWrapWord", #{const PANGO_WRAP_WORD}),
	("PangoWrapChar", #{const PANGO_WRAP_CHAR}),
	("PangoWrapWordChar", #{const PANGO_WRAP_WORD_CHAR}) ]

instance PangoLayoutSetting PangoWrapMode where
	pangoLayoutSet = pangoLayoutSetWrap
	pangoLayoutGet = pangoLayoutGetWrap

pangoLayoutSetWrap ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> PangoWrapMode -> m ()
pangoLayoutSetWrap (PangoLayoutPrim fl) (PangoWrapMode wm) = unsafeIOToPrim
	$ withForeignPtr fl \pl -> c_pango_layout_set_wrap pl wm

pangoLayoutGetWrap :: PangoLayout -> PangoWrapMode
pangoLayoutGetWrap (PangoLayout_ fl) = unsafePerformIO $ PangoWrapMode
	<$> withForeignPtr fl c_pango_layout_get_wrap

foreign import ccall "pango_layout_set_wrap" c_pango_layout_set_wrap ::
	Ptr PangoLayout -> #{type PangoWrapMode} -> IO ()

foreign import ccall "pango_layout_get_wrap" c_pango_layout_get_wrap ::
	Ptr PangoLayout -> IO #{type PangoWrapMode}

enum "PangoEllipsizeMode" ''#{type PangoEllipsizeMode} [''Show] [
	("PangoEllipsizeNone", #{const PANGO_ELLIPSIZE_NONE}),
	("PangoEllipsizeStart", #{const PANGO_ELLIPSIZE_START}),
	("PangoEllipsizeMiddle", #{const PANGO_ELLIPSIZE_MIDDLE}),
	("PangoEllipsizeEnd", #{const PANGO_ELLIPSIZE_END}) ]

instance PangoLayoutSetting PangoEllipsizeMode where
	pangoLayoutSet = pangoLayoutSetEllipsize
	pangoLayoutGet = pangoLayoutGetEllipsize

pangoLayoutSetEllipsize :: PrimMonad m =>
	PangoLayoutPrim (PrimState m) -> PangoEllipsizeMode -> m ()
pangoLayoutSetEllipsize (PangoLayoutPrim fpl) (PangoEllipsizeMode pem) = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> c_pango_layout_set_ellipsize pl pem

pangoLayoutGetEllipsize :: PangoLayout -> PangoEllipsizeMode
pangoLayoutGetEllipsize (PangoLayout_ fl) = unsafePerformIO $ PangoEllipsizeMode
	<$> withForeignPtr fl c_pango_layout_get_ellipsize

foreign import ccall "pango_layout_set_ellipsize" c_pango_layout_set_ellipsize ::
	Ptr PangoLayout -> #{type PangoEllipsizeMode} -> IO ()

foreign import ccall "pango_layout_get_ellipsize" c_pango_layout_get_ellipsize ::
	Ptr PangoLayout -> IO #{type PangoEllipsizeMode}

newtype Indent = Indent { getIndent :: PangoFixed } deriving Show

instance PangoLayoutSetting Indent where
	pangoLayoutSet l = pangoLayoutSetIndent l . toCInt . getIndent
	pangoLayoutGet l = Indent . fromCInt $ pangoLayoutGetIndent l

pangoLayoutSetIndent ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> CInt -> m ()
pangoLayoutSetIndent (PangoLayoutPrim fl) idt = unsafeIOToPrim
	$ withForeignPtr fl \pl -> c_pango_layout_set_indent pl idt

pangoLayoutGetIndent :: PangoLayout -> CInt
pangoLayoutGetIndent (PangoLayout_ fl) = unsafePerformIO
	$ withForeignPtr fl c_pango_layout_get_indent

foreign import ccall "pango_layout_set_indent" c_pango_layout_set_indent ::
	Ptr PangoLayout -> CInt -> IO ()

foreign import ccall "pango_layout_get_indent" c_pango_layout_get_indent ::
	Ptr PangoLayout -> IO CInt

newtype Spacing = Spacing { getSpacing :: PangoFixed } deriving Show

instance PangoLayoutSetting Spacing where
	pangoLayoutSet l = pangoLayoutSetSpacing l . toCInt . getSpacing
	pangoLayoutGet l = Spacing . fromCInt $ pangoLayoutGetSpacing l

pangoLayoutSetSpacing ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> CInt -> m ()
pangoLayoutSetSpacing (PangoLayoutPrim fl) sp = unsafeIOToPrim
	$ withForeignPtr fl \pl -> c_pango_layout_set_spacing pl sp

pangoLayoutGetSpacing :: PangoLayout -> CInt
pangoLayoutGetSpacing (PangoLayout_ fl) = unsafePerformIO
	$ withForeignPtr fl c_pango_layout_get_spacing

foreign import ccall "pango_layout_set_spacing" c_pango_layout_set_spacing ::
	Ptr PangoLayout -> CInt -> IO ()

foreign import ccall "pango_layout_get_spacing" c_pango_layout_get_spacing ::
	Ptr PangoLayout -> IO CInt

newtype LineSpacing = LineSpacing { getLineSpacing :: CFloat } deriving Show

instance PangoLayoutSetting LineSpacing where
	pangoLayoutSet l = pangoLayoutSetLineSpacing l . getLineSpacing
	pangoLayoutGet l = LineSpacing $ pangoLayoutGetLineSpacing l

pangoLayoutSetLineSpacing ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> CFloat -> m ()
pangoLayoutSetLineSpacing (PangoLayoutPrim fpl) fct = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> c_pango_layout_set_line_spacing pl fct

pangoLayoutGetLineSpacing :: PangoLayout -> CFloat
pangoLayoutGetLineSpacing (PangoLayout_ fl) = unsafePerformIO
	$ withForeignPtr fl c_pango_layout_get_line_spacing

foreign import ccall "pango_layout_set_line_spacing" c_pango_layout_set_line_spacing ::
	Ptr PangoLayout -> CFloat -> IO ()

foreign import ccall "pango_layout_get_line_spacing" c_pango_layout_get_line_spacing ::
	Ptr PangoLayout -> IO CFloat

newtype Justify = Justify { getJustify :: Bool } deriving Show

instance PangoLayoutSetting Justify where
	pangoLayoutSet l = pangoLayoutSetJustify l . getJustify
	pangoLayoutGet l = Justify $ pangoLayoutGetJustify l

pangoLayoutSetJustify ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> Bool -> m ()
pangoLayoutSetJustify (PangoLayoutPrim fl) b = unsafeIOToPrim $ withForeignPtr fl \pl ->
	c_pango_layout_set_justify pl $ bool #{const FALSE} #{const TRUE} b

pangoLayoutGetJustify :: PangoLayout -> Bool
pangoLayoutGetJustify (PangoLayout_ fl) = unsafePerformIO
	$ (<$> withForeignPtr fl c_pango_layout_get_justify) \case
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
	pangoLayoutGet l = AutoDir $ pangoLayoutGetAutoDir l

pangoLayoutSetAutoDir ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> Bool -> m ()
pangoLayoutSetAutoDir (PangoLayoutPrim fl) b = unsafeIOToPrim $ withForeignPtr fl \pl ->
	c_pango_layout_set_auto_dir pl $ bool #{const FALSE} #{const TRUE} b

pangoLayoutGetAutoDir :: PangoLayout -> Bool
pangoLayoutGetAutoDir (PangoLayout_ fl) = unsafePerformIO
	$ (<$> withForeignPtr fl c_pango_layout_get_auto_dir) \case
		#{const FALSE} -> False
		#{const TRUE} -> True
		_ -> error "never occur"

foreign import ccall "pango_layout_set_auto_dir" c_pango_layout_set_auto_dir ::
	Ptr PangoLayout -> #{type gboolean} -> IO ()

foreign import ccall "pango_layout_get_auto_dir" c_pango_layout_get_auto_dir ::
	Ptr PangoLayout -> IO #{type gboolean}

enum "PangoAlignment" ''#{type PangoAlignment} [''Show] [
	("PangoAlignLeft", #{const PANGO_ALIGN_LEFT}),
	("PangoAlignCenter", #{const PANGO_ALIGN_CENTER}),
	("PangoAlignRight", #{const PANGO_ALIGN_RIGHT}) ]

instance PangoLayoutSetting PangoAlignment where
	pangoLayoutSet = pangoLayoutSetAlignment
	pangoLayoutGet = pangoLayoutGetAlignment

pangoLayoutSetAlignment ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> PangoAlignment -> m ()
pangoLayoutSetAlignment (PangoLayoutPrim fpl) (PangoAlignment pa) = unsafeIOToPrim
	$ withForeignPtr fpl \pl -> c_pango_layout_set_alignment pl pa

pangoLayoutGetAlignment :: PangoLayout -> PangoAlignment
pangoLayoutGetAlignment (PangoLayout_ fl) = unsafePerformIO
	$ PangoAlignment <$> withForeignPtr fl c_pango_layout_get_alignment

foreign import ccall "pango_layout_set_alignment" c_pango_layout_set_alignment ::
	Ptr PangoLayout -> #{type PangoAlignment} -> IO ()

foreign import ccall "pango_layout_get_alignment" c_pango_layout_get_alignment ::
	Ptr PangoLayout -> IO #{type PangoAlignment}

instance PangoLayoutSetting PangoTabArrayNullable where
	pangoLayoutSet = pangoLayoutSetTabs
	pangoLayoutGet = pangoLayoutGetTabs

pangoLayoutSetTabs ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> PangoTabArrayNullable -> m ()
pangoLayoutSetTabs (PangoLayoutPrim fl) ta = unsafeIOToPrim
	$ withForeignPtr fl \pl -> case ta of
		PangoTabArrayNull -> c_pango_layout_set_tabs pl nullPtr
		PangoTabArrayNotNull fta -> do
			addForeignPtrFinalizer fl $ touchForeignPtr fta
			withForeignPtr fta $ c_pango_layout_set_tabs pl

pangoLayoutGetTabs :: PangoLayout -> PangoTabArrayNullable
pangoLayoutGetTabs (PangoLayout_ fl) = unsafePerformIO
	$ makePangoTabArrayNullable =<< withForeignPtr fl c_pango_layout_get_tabs

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
		SingleParagraphMode $ pangoLayoutGetSingleParagraphMode l

pangoLayoutSetSingleParagraphMode ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> Bool -> m ()
pangoLayoutSetSingleParagraphMode (PangoLayoutPrim fl) spm = unsafeIOToPrim
	$ withForeignPtr fl \pl ->
		c_pango_layout_set_single_paragraph_mode pl (boolToGboolean spm)

pangoLayoutGetSingleParagraphMode :: PangoLayout -> Bool
pangoLayoutGetSingleParagraphMode (PangoLayout_ fl) = unsafePerformIO
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
pangoLayoutGetCharacterCount (PangoLayout_ fpl) =
	withForeignPtr fpl c_pango_layout_get_character_count

foreign import ccall "pango_layout_get_character_count"
	c_pango_layout_get_character_count :: Ptr PangoLayout -> IO CInt

newtype IsEllipsized = IsEllipsized Bool deriving Show

instance PangoLayoutInfo IsEllipsized where
	pangoLayoutInfo = (IsEllipsized <$>) . pangoLayoutIsEllipsized

pangoLayoutIsEllipsized :: PangoLayout -> IO Bool
pangoLayoutIsEllipsized (PangoLayout_ fl) =
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
pangoLayoutIsWrapped (PangoLayout_ fl) =
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
pangoLayoutGetUnknownGlyphsCount (PangoLayout_ fpl) =
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
	pangoLayoutInfo = pure . pangoLayoutGetLogAttrs

data PangoLogAttrs = PangoLogAttrs (ForeignPtr PangoLogAttr) CInt deriving Show

mkPangoLogAttrs :: Ptr PangoLogAttr -> CInt -> IO PangoLogAttrs
mkPangoLogAttrs p n =
	(`PangoLogAttrs` n) <$> newForeignPtr p (c_g_free_pango_log_attr p)

foreign import ccall "g_free" c_g_free_pango_log_attr ::
	Ptr PangoLogAttr -> IO ()

pangoLayoutGetLogAttrs :: PangoLayout -> PangoLogAttrs
pangoLayoutGetLogAttrs (PangoLayout_ fl) = unsafePerformIO
	$ withForeignPtr fl \pl -> alloca \plas -> alloca \pn -> do
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

instance PangoLayoutInfo Extents where
	pangoLayoutInfo = (uncurry Extents <$>) . pangoLayoutGetExtents

pangoLayoutGetExtents :: PangoLayout -> IO (PangoRectangleFixed, PangoRectangleFixed)
pangoLayoutGetExtents (PangoLayout_ fpl) = withForeignPtr fpl \pl -> do
	irct <- mallocBytes #{size PangoRectangle}
	lrct <- mallocBytes #{size PangoRectangle}
	c_pango_layout_get_extents pl irct lrct
	(,)	<$> (PangoRectangleFixed_ <$> newForeignPtr irct (free irct))
		<*> (PangoRectangleFixed_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_get_extents" c_pango_layout_get_extents ::
	Ptr PangoLayout -> Ptr PangoRectangleFixed -> Ptr PangoRectangleFixed -> IO ()

instance PangoLayoutInfo PixelExtents where
	pangoLayoutInfo = (uncurry PixelExtents <$>) . pangoLayoutGetPixelExtents

pangoLayoutGetPixelExtents :: PangoLayout -> IO (PangoRectanglePixel, PangoRectanglePixel)
pangoLayoutGetPixelExtents (PangoLayout_ fpl) =
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
pangoLayoutGetSize (PangoLayout_ fpl) =
	withForeignPtr fpl \pl -> alloca \pw -> alloca \ph -> do
		c_pango_layout_get_size pl pw ph
		(\w h -> (fromCInt w, fromCInt h)) <$> peek pw <*> peek ph

foreign import ccall "pango_layout_get_size" c_pango_layout_get_size ::
	Ptr PangoLayout -> Ptr CInt -> Ptr CInt -> IO ()

data LayoutPixelSize = LayoutPixelSize {
	layoutPixelSizeWidth :: CInt, layoutPixelSizeHeight :: CInt }
	deriving Show

instance PangoLayoutInfo LayoutPixelSize where
	pangoLayoutInfo = (uncurry LayoutPixelSize <$>) . pangoLayoutGetPixelSize

pangoLayoutGetPixelSize :: PangoLayout -> IO (CInt, CInt)
pangoLayoutGetPixelSize (PangoLayout_ fpl) =
	withForeignPtr fpl \pl -> alloca \w -> alloca \h -> do
		c_pango_layout_get_pixel_size pl w h
		(,) <$> peek w <*> peek h

foreign import ccall "pango_layout_get_pixel_size" c_pango_layout_get_pixel_size ::
	Ptr PangoLayout -> Ptr CInt -> Ptr CInt -> IO ()

newtype Baseline = Baseline PangoFixed deriving Show

instance PangoLayoutInfo Baseline where
	pangoLayoutInfo = (Baseline . fromCInt <$>) . pangoLayoutGetBaseline

pangoLayoutGetBaseline :: PangoLayout -> IO CInt
pangoLayoutGetBaseline (PangoLayout_ fpl) =
	withForeignPtr fpl c_pango_layout_get_baseline

foreign import ccall "pango_layout_get_baseline" c_pango_layout_get_baseline ::
	Ptr PangoLayout -> IO CInt

newtype LineCount = LineCount CInt deriving Show

instance PangoLayoutInfo LineCount where
	pangoLayoutInfo = (LineCount <$>) . pangoLayoutGetLineCount

pangoLayoutGetLineCount :: PangoLayout -> IO CInt
pangoLayoutGetLineCount (PangoLayout_ fpl) =
	withForeignPtr fpl c_pango_layout_get_line_count

foreign import ccall "pango_layout_get_line_count" c_pango_layout_get_line_count ::
	Ptr PangoLayout -> IO CInt

pangoLayoutIndexToPos :: PangoLayout -> Int -> IO (Maybe PangoRectangleFixed)
pangoLayoutIndexToPos (PangoLayout_ fl) idx = withForeignPtr fl \pl -> do
	pos <- mallocBytes #{size PangoRectangle}
	t <- c_pango_layout_get_text pl
	is <- byteIndices =<< toCStringLen t
	case is `maybeIndex` idx of
		Nothing -> pure Nothing
		Just i -> do
			c_pango_layout_index_to_pos pl (fromIntegral i) pos
			Just . PangoRectangleFixed_ <$> newForeignPtr pos (free pos)

maybeIndex :: [a] -> Int -> Maybe a
maybeIndex _ i | i < 0 = Nothing
maybeIndex [] _ = Nothing
maybeIndex (x : _) 0 = Just x
maybeIndex (_ : xs) i = maybeIndex xs (i - 1)

foreign import ccall "pango_layout_index_to_pos" c_pango_layout_index_to_pos ::
	Ptr PangoLayout -> CInt -> Ptr PangoRectangleFixed -> IO ()

pangoLayoutIndexToLineX :: PangoLayout -> Int -> Bool -> IO (Maybe (CInt, PangoFixed))
pangoLayoutIndexToLineX (PangoLayout_ fpl) idx tr =
	withForeignPtr fpl \pl -> alloca \ln -> alloca \xpos -> do
		t <- c_pango_layout_get_text pl
		is <- byteIndices =<< toCStringLen t
		case is `maybeIndex` idx of
			Nothing -> pure Nothing
			Just i -> do
				c_pango_layout_index_to_line_x pl (fromIntegral i) (boolToGboolean tr) ln xpos
				Just <$> ((,) <$> peek ln <*> (fromCInt <$> peek xpos))

foreign import ccall "pango_layout_index_to_line_x"
	c_pango_layout_index_to_line_x ::
	Ptr PangoLayout -> CInt -> #{type gboolean} -> Ptr CInt -> Ptr CInt -> IO ()

pangoLayoutXyToIndex :: PangoLayout -> PangoFixed -> PangoFixed -> IO (Int, CInt, Bool)
pangoLayoutXyToIndex (PangoLayout_ fpl) x_ y_ =
	withForeignPtr fpl \pl -> alloca \idx -> alloca \tr -> do
		t <- c_pango_layout_get_text pl
		is <- byteIndices =<< toCStringLen t
		isd <- c_pango_layout_xy_to_index pl x y idx tr
		(,,) <$> (fromJust . (`elemIndex` is) . fromIntegral <$> peek idx) <*> peek tr <*> pure (gbooleanToBool isd)
	where
	[x, y] = toCInt <$> [x_, y_]

foreign import ccall "pango_layout_xy_to_index" c_pango_layout_xy_to_index ::
	Ptr PangoLayout -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO #type gboolean

pangoLayoutGetCursorPos :: PangoLayout -> Int -> IO (Maybe (PangoRectangleFixed, PangoRectangleFixed))
pangoLayoutGetCursorPos (PangoLayout_ fpl) idx = withForeignPtr fpl \pl -> do
	spos <- mallocBytes #{size PangoRectangle}
	wpos <- mallocBytes #{size PangoRectangle}
	t <- c_pango_layout_get_text pl
	is <- byteIndices =<< toCStringLen t
	case is `maybeIndex` idx of
		Nothing -> pure Nothing
		Just i -> do
			c_pango_layout_get_cursor_pos pl (fromIntegral i) spos wpos
			(Just <$>) $ (,)
				<$> (PangoRectangleFixed_ <$> newForeignPtr spos (free spos))
				<*> (PangoRectangleFixed_ <$> newForeignPtr wpos (free wpos))

foreign import ccall "pango_layout_get_cursor_pos" c_pango_layout_get_cursor_pos ::
	Ptr PangoLayout -> CInt -> Ptr PangoRectangleFixed -> Ptr PangoRectangleFixed -> IO ()

data MinMax a = Min | Jst a | Max deriving Show
data Dir = L | R deriving Show

pangoLayoutMoveCursorVisually ::
	PangoLayout -> Bool -> Int -> Bool -> Dir -> IO (Maybe (MinMax Int, CInt))
pangoLayoutMoveCursorVisually (PangoLayout_ fpl) str oidx otr dir =
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
