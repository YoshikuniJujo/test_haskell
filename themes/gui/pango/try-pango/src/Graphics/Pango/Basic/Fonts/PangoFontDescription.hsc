{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts.PangoFontDescription (
	-- * TYPE
	-- ** PangoFontDescription
	PangoFontDescription,

	-- ** PangoFontDescriptionNullable
	PangoFontDescriptionNullable,
	pangoFontDescriptionFromNullable, pangoFontDescriptionToNullable,

	-- ** PangoFontDescriptionPrim
	PangoFontDescriptionPrim,
	PangoFontDescriptionST, PangoFontDescriptionIO,
	pangoFontDescriptionFreeze, pangoFontDescriptionThaw,
	pangoFontDescriptionCopy,

	-- * FUNCTION
	pangoFontDescriptionNew,
	PangoFontDescriptionSetting,
	pangoFontDescriptionSet, pangoFontDescriptionGet,
	pangoFontDescriptionUnset,
	pangoFontDescriptionMerge, pangoFontDescriptionBetterMatch,
	pangoFontDescriptionToString, pangoFontDescriptionToFilename,

	-- * SETTING
	-- ** Family
	Family(..),

	-- ** Style
	PangoStyle(..),
	pattern PangoStyleNormal, pattern PangoStyleOblique,
	pattern PangoStyleItalic,

	-- ** Variant
	PangoVariant(..),
	pattern PangoVariantNormal, pattern PangoVariantSmallCaps,

	-- ** Weight
	PangoWeight(..),
	pattern PangoWeightThin, pattern PangoWeightUltralight,
	pattern PangoWeightLight, pattern PangoWeightSemilight,
	pattern PangoWeightBook, pattern PangoWeightNormal,
	pattern PangoWeightMedium, pattern PangoWeightSemibold,
	pattern PangoWeightBold, pattern PangoWeightUltrabold,
	pattern PangoWeightHeavy, pattern PangoWeightUltraheavy,

	-- ** Stretch
	PangoStretch(..),
	pattern PangoStretchUltraCondensed, pattern PangoStretchExtraCondensed,
	pattern PangoStretchCondensed, pattern PangoStretchSemiCondensed,
	pattern PangoStretchNormal, pattern PangoStretchSemiExpanded,
	pattern PangoStretchExpanded, pattern PangoStretchExtraExpanded,
	pattern PangoStretchUltraExpanded,

	-- ** Size
	Size(..)

	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum
import Control.Monad.Primitive
import Data.Bits
import Data.Bool
import Data.Word
import Data.Int

import System.Glib.Bool
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.VerticalText

import System.IO.Unsafe

#include <pango/pango.h>

enum "PangoFontMask" ''#{type PangoFontMask} [''Show] [
	("PangoFontMaskFamily", #{const PANGO_FONT_MASK_FAMILY}),
	("PangoFontMaskStyle", #{const PANGO_FONT_MASK_STYLE}),
	("PangoFontMaskVariant", #{const PANGO_FONT_MASK_VARIANT}),
	("PangoFontMaskWeight", #{const PANGO_FONT_MASK_WEIGHT}),
	("PangoFontMaskStretch", #{const PANGO_FONT_MASK_STRETCH}),
	("PangoFontMaskSize", #{const PANGO_FONT_MASK_SIZE}),
	("PangoFontMaskGravity", #{const PANGO_FONT_MASK_GRAVITY}),
	("PangoFontMaskVariations", #{const PANGO_FONT_MASK_VARIATIONS}) ]

pangoFontDescriptionNew ::
	PrimMonad m => m (PangoFontDescriptionPrim (PrimState m))
pangoFontDescriptionNew = unsafeIOToPrim
	$ mkPangoFontDescriptionPrim =<< c_pango_font_description_new

foreign import ccall "pango_font_description_new"
	c_pango_font_description_new :: IO (Ptr PangoFontDescription)

class PangoFontDescriptionSetting s where
	pangoFontDescriptionSet :: PrimMonad m =>
		PangoFontDescriptionPrim (PrimState m) -> s -> m ()
	pangoFontDescriptionGetUnsafe :: PangoFontDescription -> s
	pangoFontDescriptionMaskBit :: PangoFontMask

pangoFontDescriptionGet :: forall s . PangoFontDescriptionSetting s =>
	PangoFontDescription -> Maybe s
pangoFontDescriptionGet = \case
	fd -> let
		PangoFontMask fm = pangoFontDescriptionGetSetFields fd
		PangoFontMask mb = pangoFontDescriptionMaskBit @s in
		bool Nothing (Just $ pangoFontDescriptionGetUnsafe fd) $ fm .&. mb /= zeroBits

pangoFontDescriptionGetSetFields :: PangoFontDescription -> PangoFontMask
pangoFontDescriptionGetSetFields (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoFontMask <$> c_pango_font_description_get_set_fields pfd

foreign import ccall "pango_font_description_get_set_fields"
	c_pango_font_description_get_set_fields ::
	Ptr PangoFontDescription -> IO #type PangoFontMask

pangoFontDescriptionUnset :: forall s m . (PangoFontDescriptionSetting s, PrimMonad m) =>
	PangoFontDescriptionPrim (PrimState m) -> m ()
pangoFontDescriptionUnset fd = pangoFontDescriptionUnsetFields fd (pangoFontDescriptionMaskBit @s)

pangoFontDescriptionUnsetFields :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoFontMask -> m ()
pangoFontDescriptionUnsetFields (PangoFontDescriptionPrim fpfd) (PangoFontMask msk) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_unset_fields pfd msk

foreign import ccall "pango_font_description_unset_fields"
	c_pango_font_description_unset_fields ::
	Ptr PangoFontDescription -> #{type PangoFontMask} -> IO ()

newtype Family = Family String deriving Show

instance PangoFontDescriptionSetting Family where
	pangoFontDescriptionSet fd (Family f) = pangoFontDescriptionSetFamily fd f
	pangoFontDescriptionGetUnsafe fd = Family $ pangoFontDescriptionGetFamily fd
	pangoFontDescriptionMaskBit = PangoFontMaskFamily

pangoFontDescriptionSetFamily :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> String -> m ()
pangoFontDescriptionSetFamily (PangoFontDescriptionPrim fpfd) f = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> withCString f \cf ->
		c_pango_font_description_set_family pfd cf

foreign import ccall "pango_font_description_set_family" c_pango_font_description_set_family ::
	Ptr PangoFontDescription -> CString -> IO ()

pangoFontDescriptionGetFamily :: PangoFontDescription  -> String
pangoFontDescriptionGetFamily (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_get_family pfd

foreign import ccall "pango_font_description_get_family" c_pango_font_description_get_family ::
	Ptr PangoFontDescription -> IO CString

enum "PangoStyle" ''#{type PangoStyle} [''Show] [
	("PangoStyleNormal", #{const PANGO_STYLE_NORMAL}),
	("PangoStyleOblique", #{const PANGO_STYLE_OBLIQUE}),
	("PangoStyleItalic", #{const PANGO_STYLE_ITALIC}) ]

instance PangoFontDescriptionSetting PangoStyle where
	pangoFontDescriptionSet = pangoFontDescriptionSetStyle
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetStyle
	pangoFontDescriptionMaskBit = PangoFontMaskStyle

pangoFontDescriptionSetStyle :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoStyle -> m ()
pangoFontDescriptionSetStyle (PangoFontDescriptionPrim fpfd) (PangoStyle s) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_style pfd s

foreign import ccall "pango_font_description_set_style" c_pango_font_description_set_style ::
	Ptr PangoFontDescription -> #{type PangoStyle} -> IO ()

pangoFontDescriptionGetStyle :: PangoFontDescription -> PangoStyle
pangoFontDescriptionGetStyle (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd -> PangoStyle <$> c_pango_font_description_get_style pfd

foreign import ccall "pango_font_description_get_style" c_pango_font_description_get_style ::
	Ptr PangoFontDescription -> IO #type PangoStyle

enum "PangoVariant" ''#{type PangoVariant} [''Show] [
	("PangoVariantNormal", #{const PANGO_VARIANT_NORMAL}),
	("PangoVariantSmallCaps", #{const PANGO_VARIANT_SMALL_CAPS}) ]

instance PangoFontDescriptionSetting PangoVariant where
	pangoFontDescriptionSet = pangoFontDescriptionSetVariant
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetVariant
	pangoFontDescriptionMaskBit = PangoFontMaskVariant

pangoFontDescriptionSetVariant :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoVariant -> m ()
pangoFontDescriptionSetVariant (PangoFontDescriptionPrim fpfd) (PangoVariant pv) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_variant pfd pv

foreign import ccall "pango_font_description_set_variant" c_pango_font_description_set_variant ::
	Ptr PangoFontDescription -> #{type PangoVariant} -> IO ()

pangoFontDescriptionGetVariant :: PangoFontDescription -> PangoVariant
pangoFontDescriptionGetVariant (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoVariant <$> c_pango_font_description_get_variant pfd

foreign import ccall "pango_font_description_get_variant" c_pango_font_description_get_variant ::
	Ptr PangoFontDescription -> IO #type PangoVariant

enum "PangoWeight" ''#{type PangoWeight} [''Show] [
	("PangoWeightThin", #{const PANGO_WEIGHT_THIN}),
	("PangoWeightUltralight", #{const PANGO_WEIGHT_ULTRALIGHT}),
	("PangoWeightLight", #{const PANGO_WEIGHT_LIGHT}),
	("PangoWeightSemilight", #{const PANGO_WEIGHT_SEMILIGHT}),
	("PangoWeightBook", #{const PANGO_WEIGHT_BOOK}),
	("PangoWeightNormal", #{const PANGO_WEIGHT_NORMAL}),
	("PangoWeightMedium", #{const PANGO_WEIGHT_MEDIUM}),
	("PangoWeightSemibold", #{const PANGO_WEIGHT_SEMIBOLD}),
	("PangoWeightBold", #{const PANGO_WEIGHT_BOLD}),
	("PangoWeightUltrabold", #{const PANGO_WEIGHT_ULTRABOLD}),
	("PangoWeightHeavy", #{const PANGO_WEIGHT_HEAVY}),
	("PangoWeightUltraheavy", #{const PANGO_WEIGHT_ULTRAHEAVY}) ]

instance PangoFontDescriptionSetting PangoWeight where
	pangoFontDescriptionSet = pangoFontDescriptionSetWeight
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetWeight
	pangoFontDescriptionMaskBit = PangoFontMaskWeight

pangoFontDescriptionSetWeight :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoWeight -> m ()
pangoFontDescriptionSetWeight (PangoFontDescriptionPrim fpfd) (PangoWeight wt) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_weight pfd wt

foreign import ccall "pango_font_description_set_weight" c_pango_font_description_set_weight ::
	Ptr PangoFontDescription -> #{type PangoWeight} -> IO ()

pangoFontDescriptionGetWeight :: PangoFontDescription -> PangoWeight
pangoFontDescriptionGetWeight (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoWeight <$> c_pango_font_description_get_weight pfd

foreign import ccall "pango_font_description_get_weight" c_pango_font_description_get_weight ::
	Ptr PangoFontDescription -> IO #type PangoWeight

enum "PangoStretch" ''#{type PangoStretch} [''Show] [
	("PangoStretchUltraCondensed", #{const PANGO_STRETCH_ULTRA_CONDENSED}),
	("PangoStretchExtraCondensed", #{const PANGO_STRETCH_EXTRA_CONDENSED}),
	("PangoStretchCondensed", #{const PANGO_STRETCH_CONDENSED}),
	("PangoStretchSemiCondensed", #{const PANGO_STRETCH_SEMI_CONDENSED}),
	("PangoStretchNormal", #{const PANGO_STRETCH_NORMAL}),
	("PangoStretchSemiExpanded", #{const PANGO_STRETCH_SEMI_EXPANDED}),
	("PangoStretchExpanded", #{const PANGO_STRETCH_EXPANDED}),
	("PangoStretchExtraExpanded", #{const PANGO_STRETCH_EXTRA_EXPANDED}),
	("PangoStretchUltraExpanded", #{const PANGO_STRETCH_ULTRA_EXPANDED}) ]

instance PangoFontDescriptionSetting PangoStretch where
	pangoFontDescriptionSet = pangoFontDescriptionSetStretch
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetStretch
	pangoFontDescriptionMaskBit = PangoFontMaskStretch

pangoFontDescriptionSetStretch :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoStretch -> m ()
pangoFontDescriptionSetStretch (PangoFontDescriptionPrim fpfd) (PangoStretch ps) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_stretch pfd ps

foreign import ccall "pango_font_description_set_stretch" c_pango_font_description_set_stretch ::
	Ptr PangoFontDescription -> #{type PangoStretch} -> IO ()

pangoFontDescriptionGetStretch :: PangoFontDescription -> PangoStretch
pangoFontDescriptionGetStretch (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd -> PangoStretch <$> c_pango_font_description_get_stretch pfd

foreign import ccall "pango_font_description_get_stretch" c_pango_font_description_get_stretch ::
	Ptr PangoFontDescription -> IO #type PangoStretch

data Size = Size CDouble | AbsoluteSize CDouble deriving Show

instance PangoFontDescriptionSetting Size where
	pangoFontDescriptionSet fd = \case
		Size s -> pangoFontDescriptionSetSize fd . round $ s * #{const PANGO_SCALE}
		AbsoluteSize as -> pangoFontDescriptionSetAbsoluteSize fd $ as * #{const PANGO_SCALE}
	pangoFontDescriptionGetUnsafe fd =
		let	a = pangoFontDescriptionGetSizeIsAbsolute fd
			s = pangoFontDescriptionGetSize fd in
		bool (Size $ fromIntegral s / #{const PANGO_SCALE}) (AbsoluteSize $ fromIntegral s / #{const PANGO_SCALE}) a
	pangoFontDescriptionMaskBit = PangoFontMaskSize

pangoFontDescriptionSetSize :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> CInt -> m ()
pangoFontDescriptionSetSize (PangoFontDescriptionPrim fpfd) n = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_size pfd n

foreign import ccall "pango_font_description_set_size" c_pango_font_description_set_size ::
	Ptr PangoFontDescription -> CInt -> IO ()

pangoFontDescriptionGetSize :: PangoFontDescription -> CInt
pangoFontDescriptionGetSize (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd c_pango_font_description_get_size

foreign import ccall "pango_font_description_get_size" c_pango_font_description_get_size ::
	Ptr PangoFontDescription -> IO CInt

pangoFontDescriptionSetAbsoluteSize :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> CDouble -> m ()
pangoFontDescriptionSetAbsoluteSize (PangoFontDescriptionPrim fpfd) sz = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_absolute_size pfd sz

foreign import ccall "pango_font_description_set_absolute_size" c_pango_font_description_set_absolute_size ::
	Ptr PangoFontDescription -> CDouble -> IO ()

pangoFontDescriptionGetSizeIsAbsolute :: PangoFontDescription -> Bool
pangoFontDescriptionGetSizeIsAbsolute (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		gbooleanToBool <$> c_pango_font_description_get_size_is_absolute pfd

foreign import ccall "pango_font_description_get_size_is_absolute"
	c_pango_font_description_get_size_is_absolute ::
	Ptr PangoFontDescription -> IO #type gboolean

instance PangoFontDescriptionSetting PangoGravity where
	pangoFontDescriptionSet = pangoFontDescriptionSetGravity
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetGravity
	pangoFontDescriptionMaskBit = PangoFontMaskGravity

pangoFontDescriptionSetGravity :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoGravity -> m ()
pangoFontDescriptionSetGravity (PangoFontDescriptionPrim fpfd) (PangoGravity gr) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_gravity pfd gr

foreign import ccall "pango_font_description_set_gravity"
	c_pango_font_description_set_gravity ::
	Ptr PangoFontDescription -> #{type PangoGravity} -> IO ()

pangoFontDescriptionGetGravity :: PangoFontDescription -> PangoGravity
pangoFontDescriptionGetGravity (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoGravity <$> c_pango_font_description_get_gravity pfd

foreign import ccall "pango_font_description_get_gravity"
	c_pango_font_description_get_gravity ::
	Ptr PangoFontDescription -> IO #type PangoGravity

pangoFontDescriptionMerge :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) ->
	PangoFontDescriptionPrim (PrimState m) -> Bool -> m ()
pangoFontDescriptionMerge (PangoFontDescriptionPrim fdsc)
	(PangoFontDescriptionPrim fdsctm) re = unsafeIOToPrim
	$ withForeignPtr fdsc \pdsc -> withForeignPtr fdsctm \pdsctm ->
		c_pango_font_description_merge pdsc pdsctm $ boolToGboolean re

foreign import ccall "pango_font_description_merge"
	c_pango_font_description_merge ::
	Ptr PangoFontDescription -> Ptr PangoFontDescription -> #{type gboolean} -> IO ()

pangoFontDescriptionBetterMatch :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) ->
	Maybe (PangoFontDescriptionPrim (PrimState m)) ->
	PangoFontDescriptionPrim (PrimState m) -> m Bool
pangoFontDescriptionBetterMatch
	(PangoFontDescriptionPrim fdsc) mom (PangoFontDescriptionPrim fnm) = unsafeIOToPrim
	$ gbooleanToBool <$> withForeignPtr fdsc \pdsc -> withMaybePangoFontDescriptionPrim mom \pom -> withForeignPtr fnm \pnm ->
		c_pango_font_description_better_match pdsc pom pnm

withMaybePangoFontDescriptionPrim ::
	Maybe (PangoFontDescriptionPrim s) -> (Ptr PangoFontDescription -> IO a) -> IO a
withMaybePangoFontDescriptionPrim = \case
	Nothing -> ($ nullPtr)
	Just (PangoFontDescriptionPrim ffd) -> withForeignPtr ffd

foreign import ccall "pango_font_description_better_match"
	c_pango_font_description_better_match ::
	Ptr PangoFontDescription -> Ptr PangoFontDescription ->
	Ptr PangoFontDescription -> IO #{type gboolean}

pangoFontDescriptionToString :: PangoFontDescription -> String
pangoFontDescriptionToString (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_string pfd

foreign import ccall "pango_font_description_to_string"
	c_pango_font_description_to_string ::
	Ptr PangoFontDescription -> IO CString

pangoFontDescriptionToFilename :: PangoFontDescription -> String
pangoFontDescriptionToFilename (PangoFontDescription_ fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_filename pfd

foreign import ccall "pango_font_description_to_filename"
	c_pango_font_description_to_filename ::
	Ptr PangoFontDescription -> IO CString
