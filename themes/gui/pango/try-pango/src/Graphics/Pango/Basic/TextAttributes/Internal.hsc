{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.TextAttributes.Internal (
	-- * TYPE
	PangoTextAttrList(..),
	PangoTextAttrListPrim,
	pangoTextAttrListNew,
	pangoTextAttrListFreeze, pangoTextAttrListThaw,
	pangoTextAttrListCopy,

	-- * PARSE MARKUP
	pangoParseMarkup, pangoMarkupParserNew, pangoMarkupParserFinish,

	-- * INSERT AN ATTRIBUTE TO PANGO TEXT ATTRIBUTE LIST FOR PRIMITIVE MONAD
	pangoTextAttrListInsert, pangoTextAttrListInsertBefore,
	pangoTextAttrListChange,

	-- * PANGO ATTRIBUTE VALUE
	-- ** Class
	PangoAttribute, PangoAttributeValue, pangoAttrNew,

	-- ** Instance
	-- *** FontDescription
	pangoAttrFontDescNew,

	-- *** Strikethrough and StrikethroughColor
	Strikethrough(..), StrikethroughColor(..),

	-- *** PangoUnderline and UnderlineColor
	PangoUnderline, pattern PangoUnderlineNone,
	pattern PangoUnderlineSingle, pattern PangoUnderlineDouble,
	pattern PangoUnderlineLow, pattern PangoUnderlineError,
	UnderlineColor(..),

	-- *** Shape
	Shape(..),

	-- *** Scale
	Scale(..),

	-- *** Rise
	Rise, pattern Rise,

	-- *** LetterSpacing
	LetterSpacing, pattern LetterSpacing,

	-- *** Color and Alpha of Foreground and Background
	ForegroundColor(..), BackgroundColor(..),
	ForegroundAlpha(..), BackgroundAlpha(..),

	-- * PANGO COLOR
	PangoColor(..), pangoColorParse, pangoColorToString,

	-- * INTERNAL
	PangoAttrList(..), mkPangoAttrList ) where

import GHC.Stack
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.String.Utf8
import Foreign.C.String.Misc
import Foreign.C.String.ForeignCStringLen
import Foreign.C.Enum
import Control.Monad.Primitive
import Data.Array
import Data.Bool
import Data.Word
import Data.Int
import Data.Char
import System.IO.Unsafe

import System.Glib.ErrorReporting
import System.Glib.SimpleXmlSubsetParser

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

import Graphics.Pango.Basic.GlyphStorage.Internal
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage.Internal
import Graphics.Pango.Basic.VerticalText

#include <pango/pango.h>

data PangoTextAttrList = PangoTextAttrList {
	pangoTextAttrListText :: ForeignCStringLen,
	pangoTextAttrListAttrList :: PangoAttrList
	} deriving Show

data PangoAttrList
	= PangoAttrListNull
	| PangoAttrList (ForeignPtr PangoAttrList)
	deriving Show

mkPangoAttrList :: Ptr PangoAttrList -> IO PangoAttrList
mkPangoAttrList p
	| p == nullPtr = pure PangoAttrListNull
	| otherwise = PangoAttrList <$> newForeignPtr p (c_pango_attr_list_unref p)

foreign import ccall "pango_attr_list_unref" c_pango_attr_list_unref ::
	Ptr PangoAttrList -> IO ()

pangoParseMarkup :: T.Text -> Maybe Char -> Either GError (PangoTextAttrList, Maybe Char)
pangoParseMarkup mt am = unsafePerformIO
	$ T.withCStringLen mt \(cmt, cmtl) -> alloca \ppal -> alloca \pt -> alloca \pac -> alloca \pge -> do
		r <- c_pango_parse_markup cmt (fromIntegral cmtl) (toGunichar am) ppal pt pac pge
		pt' <- toCStringLen =<< peek pt
		pt'' <- copyToForeignCStringLen pt'
		case r of
			#{const FALSE} -> Left <$> (mkGError =<< peek pge)
			#{const TRUE} -> (Right <$>) $ (,)
				<$> (PangoTextAttrList pt'' <$> (mkPangoAttrList =<< peek ppal))
				<*> (fromGunichar <$> peek pac)
			_ -> error "never occur"

toGunichar :: Maybe Char -> #{type gunichar}
toGunichar = \case Nothing -> 0; Just c -> fromIntegral $ ord c

fromGunichar :: #{type gunichar} -> Maybe Char
fromGunichar = \case 0 -> Nothing; uc -> Just . chr $ fromIntegral uc

foreign import ccall "pango_parse_markup" c_pango_parse_markup ::
	CString -> CInt -> #{type gunichar} ->
		Ptr (Ptr PangoAttrList) -> Ptr CString -> Ptr #{type gunichar} ->
		Ptr (Ptr GError) -> IO #{type gboolean}

pangoMarkupParserNew :: PrimMonad m => Maybe Char -> m (GMarkupParseContext (PrimState m))
pangoMarkupParserNew am = unsafeIOToPrim
	$ mkGMarkupParseContext =<< c_pango_markup_parser_new (toGunichar am)

foreign import ccall "pango_markup_parser_new" c_pango_markup_parser_new ::
	#{type gunichar} -> IO (Ptr (GMarkupParseContext s))

pangoMarkupParserFinish :: PrimMonad m => GMarkupParseContext (PrimState m) ->
	m (Either GError (PangoTextAttrList, Maybe Char))
pangoMarkupParserFinish (GMarkupParseContext fpc) = unsafeIOToPrim
	$ withForeignPtr fpc \ppc -> alloca \ppal -> alloca \pt -> alloca \pac -> alloca \pge -> do
		r <- c_pango_markup_parser_finish ppc ppal pt pac pge
		pt' <- toCStringLen =<< peek pt
		pt'' <- copyToForeignCStringLen pt'
		case r of
			#{const FALSE} -> Left <$> (mkGError =<< peek pge)
			#{const TRUE} -> (Right <$>) $ (,)
				<$> (PangoTextAttrList pt'' <$> (mkPangoAttrList =<< peek ppal))
				<*> (fromGunichar <$> peek pac)
			_ -> error "never occur"

foreign import ccall "pango_markup_parser_finish"
	c_pango_markup_parser_finish ::
	Ptr (GMarkupParseContext s) -> Ptr (Ptr PangoAttrList) -> Ptr CString ->
	Ptr #{type gunichar} -> Ptr (Ptr GError) -> IO #{type gboolean}

newtype PangoAttribute s = PangoAttribute (ForeignPtr (PangoAttribute s))
	deriving Show

mkPangoAttribute :: Ptr (PangoAttribute s) -> IO (PangoAttribute s)
mkPangoAttribute p =
	PangoAttribute <$> newForeignPtr p (c_pango_attribute_destroy p)

foreign import ccall "pango_attribute_destroy" c_pango_attribute_destroy ::
	Ptr (PangoAttribute s) -> IO ()

pangoAttributeSetStartIndex, pangoAttributeSetEndIndex :: PrimMonad m =>
	PangoAttribute (PrimState m) -> CUInt -> m ()
pangoAttributeSetStartIndex (PangoAttribute fa) si = unsafeIOToPrim
	$ withForeignPtr fa \pa -> #{poke PangoAttribute, start_index} pa si

pangoAttributeSetEndIndex (PangoAttribute fa) ei = unsafeIOToPrim
	$ withForeignPtr fa \pa -> #{poke PangoAttribute, end_index} pa ei

class PangoAttributeValue v where
	pangoAttrNew :: PrimMonad m => v -> m (PangoAttribute (PrimState m))

instance PangoAttributeValue PangoLanguage where
	pangoAttrNew = pangoAttrLanguageNew

pangoAttrLanguageNew :: PrimMonad m =>
	PangoLanguage -> m (PangoAttribute (PrimState m))
pangoAttrLanguageNew (PangoLanguage_ l) =
	unsafeIOToPrim $ mkPangoAttribute =<< c_pango_attr_language_new l

foreign import ccall "pango_attr_language_new" c_pango_attr_language_new ::
	Ptr PangoLanguage -> IO (Ptr (PangoAttribute s))

-- data Family = Family String deriving Show

instance PangoAttributeValue Family where
	pangoAttrNew (Family f) = pangoAttrFamilyNew f

pangoAttrFamilyNew :: PrimMonad m => String -> m (PangoAttribute (PrimState m))
pangoAttrFamilyNew f =
	unsafeIOToPrim $ mkPangoAttribute =<< withCString f c_pango_attr_family_new

foreign import ccall "pango_attr_family_new" c_pango_attr_family_new ::
	CString -> IO (Ptr (PangoAttribute s))

instance PangoAttributeValue PangoStyle where pangoAttrNew = pangoAttrStyleNew

pangoAttrStyleNew :: PrimMonad m => PangoStyle -> m (PangoAttribute (PrimState m))
pangoAttrStyleNew (PangoStyle s) =
	unsafeIOToPrim $ mkPangoAttribute =<< c_pango_attr_style_new s

foreign import ccall "pango_attr_style_new" c_pango_attr_style_new ::
	#{type PangoStyle} -> IO (Ptr (PangoAttribute s))

instance PangoAttributeValue PangoVariant where
	pangoAttrNew = pangoAttrVariantNew

pangoAttrVariantNew :: PrimMonad m => PangoVariant -> m (PangoAttribute (PrimState m))
pangoAttrVariantNew (PangoVariant v) =
	unsafeIOToPrim $ mkPangoAttribute =<< c_pango_attr_variant_new v

foreign import ccall "pango_attr_variant_new" c_pango_attr_variant_new ::
	#{type PangoVariant} -> IO (Ptr (PangoAttribute s))

instance PangoAttributeValue PangoStretch where
	pangoAttrNew = pangoAttrStretchNew

pangoAttrStretchNew :: PrimMonad m => PangoStretch -> m (PangoAttribute (PrimState m))
pangoAttrStretchNew (PangoStretch s) =
	unsafeIOToPrim $ mkPangoAttribute =<< c_pango_attr_stretch_new s

foreign import ccall "pango_attr_stretch_new" c_pango_attr_stretch_new ::
	#{type PangoStretch} -> IO (Ptr (PangoAttribute s))

instance PangoAttributeValue PangoWeight where
	pangoAttrNew = pangoAttrWeightNew

pangoAttrWeightNew :: PrimMonad m => PangoWeight -> m (PangoAttribute (PrimState m))
pangoAttrWeightNew (PangoWeight w) =
	unsafeIOToPrim $ mkPangoAttribute =<< c_pango_attr_weight_new w

foreign import ccall "pango_attr_weight_new" c_pango_attr_weight_new ::
	#{type PangoWeight} -> IO (Ptr (PangoAttribute s))

-- data Size = Size Double | AbsoluteSize Double deriving Show

instance PangoAttributeValue Size where
	pangoAttrNew = pangoAttrSizeNew

pangoAttrSizeNew :: PrimMonad m => Size -> m (PangoAttribute (PrimState m))
pangoAttrSizeNew = unsafeIOToPrim . (mkPangoAttribute =<<) . \case
	Size s -> c_pango_attr_size_new . round $ s * #{const PANGO_SCALE}
	AbsoluteSize a -> c_pango_attr_size_new_absolute . round $ a * #{const PANGO_SCALE}

foreign import ccall "pango_attr_size_new" c_pango_attr_size_new ::
	CInt -> IO (Ptr (PangoAttribute s))

foreign import ccall "pango_attr_size_new_absolute" c_pango_attr_size_new_absolute ::
	CInt -> IO (Ptr (PangoAttribute s))

pangoAttrFontDescNew :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> m (PangoAttribute (PrimState m))
pangoAttrFontDescNew (PangoFontDescriptionPrim ffd) = unsafeIOToPrim
	$ mkPangoAttribute =<< withForeignPtr ffd c_pango_attr_font_desc_new

foreign import ccall "pango_attr_font_desc_new" c_pango_attr_font_desc_new ::
	Ptr PangoFontDescription -> IO (Ptr (PangoAttribute s))

data ForegroundColor = ForegroundColor Word16 Word16 Word16 deriving Show

instance PangoAttributeValue ForegroundColor where
	pangoAttrNew = pangoAttrForegroundNew

pangoAttrForegroundNew :: PrimMonad m =>
	ForegroundColor -> m (PangoAttribute (PrimState m))
pangoAttrForegroundNew (ForegroundColor r g b) = unsafeIOToPrim
	$ mkPangoAttribute =<< c_pango_attr_foreground_new r g b

foreign import ccall "pango_attr_foreground_new" c_pango_attr_foreground_new ::
	Word16 -> Word16 -> Word16 -> IO (Ptr (PangoAttribute s))

data BackgroundColor = BackgroundColor Word16 Word16 Word16 deriving Show

instance PangoAttributeValue BackgroundColor where
	pangoAttrNew = pangoAttrBackgroundNew

pangoAttrBackgroundNew :: PrimMonad m =>
	BackgroundColor -> m (PangoAttribute (PrimState m))
pangoAttrBackgroundNew (BackgroundColor r g b) = unsafeIOToPrim
	$ mkPangoAttribute =<< c_pango_attr_background_new r g b

foreign import ccall "pango_attr_background_new" c_pango_attr_background_new ::
	Word16 -> Word16 -> Word16 -> IO (Ptr (PangoAttribute s))

newtype Strikethrough = Strikethrough Bool deriving Show

instance PangoAttributeValue Strikethrough where
	pangoAttrNew (Strikethrough b) = pangoAttrStrikethroughNew b

pangoAttrStrikethroughNew :: PrimMonad m =>
	Bool -> m (PangoAttribute (PrimState m))
pangoAttrStrikethroughNew b = unsafeIOToPrim
	$ mkPangoAttribute =<< c_pango_attr_strikethrough_new case b of
		False -> #{const FALSE}; True -> #{const TRUE}

foreign import ccall "pango_attr_strikethrough_new" c_pango_attr_strikethrough_new ::
	#{type gboolean} -> IO (Ptr (PangoAttribute s))

data StrikethroughColor = StrikethroughColor Word16 Word16 Word16 deriving Show

instance PangoAttributeValue StrikethroughColor where
	pangoAttrNew = pangoAttrStrikethroughColorNew

pangoAttrStrikethroughColorNew :: PrimMonad m =>
	StrikethroughColor -> m (PangoAttribute (PrimState m))
pangoAttrStrikethroughColorNew (StrikethroughColor r g b) = unsafeIOToPrim
	$ mkPangoAttribute =<< c_pango_attr_strikethrough_color_new r g b

foreign import ccall "pango_attr_strikethrough_color_new" c_pango_attr_strikethrough_color_new ::
	Word16 -> Word16 -> Word16 -> IO (Ptr (PangoAttribute s))

enum "PangoUnderline" ''#{type PangoUnderline} [''Show] [
	("PangoUnderlineNone", #{const PANGO_UNDERLINE_NONE}),
	("PangoUnderlineSingle", #{const PANGO_UNDERLINE_SINGLE}),
	("PangoUnderlineDouble", #{const PANGO_UNDERLINE_DOUBLE}),
	("PangoUnderlineLow", #{const PANGO_UNDERLINE_LOW}),
	("PangoUnderlineError", #{const PANGO_UNDERLINE_ERROR}) ]

instance PangoAttributeValue PangoUnderline where
	pangoAttrNew = pangoAttrUnderlineNew

pangoAttrUnderlineNew :: PrimMonad m =>
	PangoUnderline -> m (PangoAttribute (PrimState m))
pangoAttrUnderlineNew (PangoUnderline ul) = unsafeIOToPrim
	$ mkPangoAttribute =<< c_pango_attr_underline_new ul

foreign import ccall "pango_attr_underline_new" c_pango_attr_underline_new ::
	#{type PangoUnderline} -> IO (Ptr (PangoAttribute s))

data UnderlineColor = UnderlineColor Word16 Word16 Word16 deriving Show

instance PangoAttributeValue UnderlineColor where
	pangoAttrNew = pangoAttrUnderlineColorNew

pangoAttrUnderlineColorNew :: PrimMonad m =>
	UnderlineColor -> m (PangoAttribute (PrimState m))
pangoAttrUnderlineColorNew (UnderlineColor r g b) = unsafeIOToPrim
	$ mkPangoAttribute =<< c_pango_attr_underline_color_new r g b

foreign import ccall "pango_attr_underline_color_new" c_pango_attr_underline_color_new ::
	Word16 -> Word16 -> Word16 -> IO (Ptr (PangoAttribute s))

data Shape = Shape PangoRectangleFixed PangoRectangleFixed deriving Show

instance PangoAttributeValue Shape where
	pangoAttrNew (Shape ir lr) = pangoAttrShapeNew ir lr

pangoAttrShapeNew :: PrimMonad m =>
	PangoRectangleFixed -> PangoRectangleFixed -> m (PangoAttribute (PrimState m))
pangoAttrShapeNew (PangoRectangleFixed_ fir) (PangoRectangleFixed_ flr) = unsafeIOToPrim
	$ withForeignPtr fir \pir -> withForeignPtr flr \plr ->
		mkPangoAttribute =<< c_pango_attr_shape_new pir plr

foreign import ccall "pango_attr_shape_new" c_pango_attr_shape_new ::
	Ptr PangoRectangleFixed -> Ptr PangoRectangleFixed -> IO (Ptr (PangoAttribute s))

newtype Scale = Scale CDouble deriving Show

instance PangoAttributeValue Scale where
	pangoAttrNew (Scale s) = pangoAttrScaleNew s

pangoAttrScaleNew :: PrimMonad m => CDouble -> m (PangoAttribute (PrimState m))
pangoAttrScaleNew s =
	unsafeIOToPrim $ mkPangoAttribute =<< c_pango_attr_scale_new s

foreign import ccall "pango_attr_scale_new" c_pango_attr_scale_new ::
	CDouble -> IO (Ptr (PangoAttribute s))

newtype Rise = RiseInPangoUnit { getRiseInPangoUnit :: CInt } deriving Show

pattern Rise :: PangoFixed -> Rise
pattern Rise r <- (fromCInt . getRiseInPangoUnit -> r) where
	Rise = RiseInPangoUnit . toCInt

instance PangoAttributeValue Rise where
	pangoAttrNew = pangoAttrRiseNew . getRiseInPangoUnit

pangoAttrRiseNew :: PrimMonad m => CInt -> m (PangoAttribute (PrimState m))
pangoAttrRiseNew r =
	unsafeIOToPrim $ mkPangoAttribute =<< c_pango_attr_rise_new r

foreign import ccall "pango_attr_rise_new" c_pango_attr_rise_new ::
	CInt -> IO (Ptr (PangoAttribute s))

newtype LetterSpacing =
	LetterSpacingInPangoUnit { getLetterSpacingInPangoUnit :: CInt }
	deriving Show

instance PangoAttributeValue LetterSpacing where
	pangoAttrNew = pangoAttrLetterSpacingNew . getLetterSpacingInPangoUnit

pattern LetterSpacing :: PangoFixed -> LetterSpacing
pattern LetterSpacing s <-
	(fromCInt . getLetterSpacingInPangoUnit -> s) where
	LetterSpacing = LetterSpacingInPangoUnit . toCInt

pangoAttrLetterSpacingNew ::
	PrimMonad m => CInt -> m (PangoAttribute (PrimState m))
pangoAttrLetterSpacingNew s =
	unsafeIOToPrim $ mkPangoAttribute =<< c_pango_attr_letter_spacing_new s

foreign import ccall "pango_attr_letter_spacing_new"
	c_pango_attr_letter_spacing_new ::
	CInt -> IO (Ptr (PangoAttribute s))

newtype Fallback = Fallback Bool deriving Show

instance PangoAttributeValue Fallback where
	pangoAttrNew (Fallback b) = pangoAttrFallbackNew b

pangoAttrFallbackNew :: PrimMonad m => Bool -> m (PangoAttribute (PrimState m))
pangoAttrFallbackNew b = unsafeIOToPrim $ mkPangoAttribute
	=<< c_pango_attr_fallback_new (bool #{const FALSE} #{const TRUE} b)

foreign import ccall "pango_attr_fallback_new" c_pango_attr_fallback_new ::
	#{type gboolean} -> IO (Ptr (PangoAttribute s))

instance PangoAttributeValue PangoGravity where
	pangoAttrNew = pangoAttrGravityNew

pangoAttrGravityNew ::
	PrimMonad m => PangoGravity -> m (PangoAttribute (PrimState m))
pangoAttrGravityNew (PangoGravity g) = unsafeIOToPrim
	$ mkPangoAttribute =<< c_pango_attr_gravity_new g

foreign import ccall "pango_attr_gravity_new" c_pango_attr_gravity_new ::
	#{type PangoGravity} -> IO (Ptr (PangoAttribute s))

instance PangoAttributeValue PangoGravityHint where
	pangoAttrNew = pangoAttrGravityHintNew

pangoAttrGravityHintNew ::
	PrimMonad m => PangoGravityHint -> m (PangoAttribute (PrimState m))
pangoAttrGravityHintNew (PangoGravityHint gh) = unsafeIOToPrim
	$ mkPangoAttribute =<< c_pango_attr_gravity_hint_new gh

foreign import ccall "pango_attr_gravity_hint_new"
	c_pango_attr_gravity_hint_new ::
	#{type PangoGravityHint} -> IO (Ptr (PangoAttribute s))

newtype FontFeatures = FontFeatures { getFontFeatures :: String } deriving Show

instance PangoAttributeValue FontFeatures where
	pangoAttrNew = pangoAttrFontFeaturesNew . getFontFeatures

pangoAttrFontFeaturesNew ::
	PrimMonad m => String -> m (PangoAttribute (PrimState m))
pangoAttrFontFeaturesNew f = unsafeIOToPrim
	$ mkPangoAttribute =<< withCString f c_pango_attr_font_features_new

foreign import ccall "pango_attr_font_features_new"
	c_pango_attr_font_features_new ::
	CString -> IO (Ptr (PangoAttribute s))

newtype ForegroundAlpha = ForegroundAlpha { getForegroundAlpha :: Word16 }
	deriving Show

instance PangoAttributeValue ForegroundAlpha where
	pangoAttrNew = pangoAttrForegroundAlphaNew . getForegroundAlpha

newtype BackgroundAlpha = BackgroundAlpha { getBackgroundAlpha :: Word16 }
	deriving Show

instance PangoAttributeValue BackgroundAlpha where
	pangoAttrNew = pangoAttrBackgroundAlphaNew . getBackgroundAlpha

pangoAttrForegroundAlphaNew, pangoAttrBackgroundAlphaNew ::
	PrimMonad m => Word16 -> m (PangoAttribute (PrimState m))
pangoAttrForegroundAlphaNew a = unsafeIOToPrim
	$ mkPangoAttribute =<< c_pango_attr_foreground_alpha_new a

pangoAttrBackgroundAlphaNew a = unsafeIOToPrim
	$ mkPangoAttribute =<< c_pango_attr_background_alpha_new a

foreign import ccall "pango_attr_foreground_alpha_new"
	c_pango_attr_foreground_alpha_new ::
	Word16 -> IO (Ptr (PangoAttribute s))

foreign import ccall "pango_attr_background_alpha_new"
	c_pango_attr_background_alpha_new ::
	Word16 -> IO (Ptr (PangoAttribute s))

data PangoColor = PangoColor {
	pangoColorRed :: Word16,
	pangoColorGreen :: Word16,
	pangoColorBlue :: Word16 } deriving Show

instance Storable PangoColor where
	sizeOf _ = #{size PangoColor}
	alignment _ = #{alignment PangoColor}
	peek p = PangoColor
		<$> #{peek PangoColor, red} p
		<*> #{peek PangoColor, green} p
		<*> #{peek PangoColor, blue} p
	poke p (PangoColor r g b) = do
		#{poke PangoColor, red} p r
		#{poke PangoColor, green} p g
		#{poke PangoColor, blue} p b

pangoColorParse :: String -> Maybe PangoColor
pangoColorParse s = unsafePerformIO $ withCString s \cs -> alloca \pc ->
	c_pango_color_parse pc cs >>= \case
		#{const FALSE} -> pure Nothing
		#{const TRUE} -> Just <$> peek pc
		_ -> error "never occur"

foreign import ccall "pango_color_parse" c_pango_color_parse ::
	Ptr PangoColor -> CString -> IO #{type gboolean}

pangoColorToString :: PangoColor -> String
pangoColorToString c = unsafePerformIO $ peekCString =<< alloca \cc ->
	poke cc c >> c_pango_color_to_string cc

foreign import ccall "pango_color_to_string" c_pango_color_to_string ::
	Ptr PangoColor -> IO CString

pangoTextAttrListFreeze :: PrimMonad m =>
	PangoTextAttrListPrim (PrimState m) -> m PangoTextAttrList
pangoTextAttrListFreeze (PangoTextAttrListPrim csl _ al) =
	PangoTextAttrList csl <$> pangoAttrListFreeze al

pangoTextAttrListThaw :: PrimMonad m =>
	PangoTextAttrList -> m (Maybe (PangoTextAttrListPrim (PrimState m)))
pangoTextAttrListThaw (PangoTextAttrList csl al) = unsafeIOToPrim do
	ids <- ((\is -> listArray (0, length is - 1) is) . (fromIntegral <$>) <$> withForeignCStringLen csl byteIndices)
	mal <- pangoAttrListThawIo al
	pure $ PangoTextAttrListPrim csl ids <$> mal

data PangoTextAttrListPrim s = PangoTextAttrListPrim {
	pangoTextAttrListPrimText :: ForeignCStringLen,
	pangoTextAttrListPrimUtf8Indices :: Array Int CUInt,
	pangoTextAttrListPrimAttrList :: PangoAttrListPrim s
	} deriving Show

pangoTextAttrListNew ::
	PrimMonad m => T.Text -> m (PangoTextAttrListPrim (PrimState m))
pangoTextAttrListNew t = unsafeIOToPrim $ T.withCStringLen t \csl -> do
	csl' <- copyToForeignCStringLen csl
	PangoTextAttrListPrim csl'
		<$> ((\is -> listArray (0, length is - 1) is) . (fromIntegral <$>)
			<$> byteIndices csl)
		<*> (mkPangoAttrListPrim =<< c_pango_attr_list_new)

pangoTextAttrListCopy :: PrimMonad m =>
	PangoTextAttrListPrim (PrimState m) -> m (PangoTextAttrListPrim (PrimState m))
pangoTextAttrListCopy (PangoTextAttrListPrim t is al) =
	PangoTextAttrListPrim t is <$> pangoAttrListCopy al

toUtf8Index :: HasCallStack => Array Int CUInt -> Int -> CUInt
toUtf8Index t i | i < mn = 0 | i > mx = maxBound | otherwise = t ! i
	where (mn, mx) = bounds t

pangoTextAttrListInsert, pangoTextAttrListInsertBefore,
	pangoTextAttrListChange :: PrimMonad m =>
	PangoTextAttrListPrim (PrimState m) ->
	PangoAttribute (PrimState m) -> Int -> Int -> m ()
pangoTextAttrListInsert = pangoTextAttrListInsertGen pangoAttrListInsert

pangoTextAttrListInsertBefore =
	pangoTextAttrListInsertGen pangoAttrListInsertBefore

pangoTextAttrListChange = pangoTextAttrListInsertGen pangoAttrListChange

pangoTextAttrListInsertGen :: PrimMonad m =>
	(PangoAttrListPrim (PrimState m) ->
		PangoAttribute (PrimState m) -> m ()) ->
	PangoTextAttrListPrim (PrimState m) -> PangoAttribute (PrimState m) ->
	Int -> Int -> m ()
pangoTextAttrListInsertGen pali (PangoTextAttrListPrim _ t al) a s e = do
	pangoAttributeSetStartIndex a (toUtf8Index t s)
	pangoAttributeSetEndIndex a (toUtf8Index t e)
	pali al a
	

newtype PangoAttrListPrim s = PangoAttrListPrim (ForeignPtr (PangoAttrListPrim s)) deriving Show

mkPangoAttrListPrim :: Ptr (PangoAttrListPrim s) -> IO (PangoAttrListPrim s)
mkPangoAttrListPrim p = PangoAttrListPrim <$> newForeignPtr p (c_pango_attr_list_prim_unref p)

foreign import ccall "pango_attr_list_unref" c_pango_attr_list_prim_unref ::
	Ptr (PangoAttrListPrim s) -> IO ()

foreign import ccall "pango_attr_list_new" c_pango_attr_list_new ::
	IO (Ptr (PangoAttrListPrim s))

pangoAttrListCopy :: PrimMonad m =>
	PangoAttrListPrim (PrimState m) -> m (PangoAttrListPrim (PrimState m))
pangoAttrListCopy (PangoAttrListPrim fal) = unsafeIOToPrim
	$ mkPangoAttrListPrim =<< withForeignPtr fal c_pango_attr_list_copy

foreign import ccall "pango_attr_list_copy" c_pango_attr_list_copy ::
	Ptr (PangoAttrListPrim s) -> IO (Ptr (PangoAttrListPrim s))

pangoAttrListFreeze ::
	PrimMonad m => PangoAttrListPrim (PrimState m) -> m PangoAttrList
pangoAttrListFreeze (PangoAttrListPrim fal) = unsafeIOToPrim
	$ mkPangoAttrList =<< withForeignPtr fal c_pango_attr_list_freeze

foreign import ccall "pango_attr_list_copy" c_pango_attr_list_freeze ::
	Ptr (PangoAttrListPrim s) -> IO (Ptr PangoAttrList)

pangoAttrListThawIo :: PangoAttrList -> IO (Maybe (PangoAttrListPrim s))
pangoAttrListThawIo = \case
	PangoAttrListNull -> pure Nothing
	PangoAttrList fal -> (Just <$>) . mkPangoAttrListPrim
		=<< withForeignPtr fal c_pango_attr_list_thaw

foreign import ccall "pango_attr_list_copy" c_pango_attr_list_thaw ::
	Ptr PangoAttrList -> IO (Ptr (PangoAttrListPrim s))

pangoAttrListInsert, pangoAttrListInsertBefore, pangoAttrListChange ::
	PrimMonad m =>
	PangoAttrListPrim (PrimState m) -> PangoAttribute (PrimState m) -> m ()
pangoAttrListInsert (PangoAttrListPrim fal) (PangoAttribute fa) = unsafeIOToPrim
	$ withForeignPtr fal \pal -> withForeignPtr fa \pa -> do
		pa' <- c_pango_attribute_copy pa
		c_pango_attr_list_insert pal pa'

pangoAttrListInsertBefore (PangoAttrListPrim fal) (PangoAttribute fa) = unsafeIOToPrim
	$ withForeignPtr fal \pal -> withForeignPtr fa \pa -> do
		pa' <- c_pango_attribute_copy pa
		c_pango_attr_list_insert_before pal pa'

pangoAttrListChange (PangoAttrListPrim fal) (PangoAttribute fa) = unsafeIOToPrim
	$ withForeignPtr fal \pal -> withForeignPtr fa \pa -> do
		pa' <- c_pango_attribute_copy pa
		c_pango_attr_list_change pal pa'

foreign import ccall "pango_attr_list_insert" c_pango_attr_list_insert ::
	Ptr (PangoAttrListPrim s) -> Ptr (PangoAttribute s) -> IO ()

foreign import ccall "pango_attr_list_insert_before" c_pango_attr_list_insert_before ::
	Ptr (PangoAttrListPrim s) -> Ptr (PangoAttribute s) -> IO ()

foreign import ccall "pango_attr_list_change" c_pango_attr_list_change ::
	Ptr (PangoAttrListPrim s) -> Ptr (PangoAttribute s) -> IO ()

foreign import ccall "pango_attribute_copy" c_pango_attribute_copy ::
	Ptr (PangoAttribute s) -> IO (Ptr (PangoAttribute s))
