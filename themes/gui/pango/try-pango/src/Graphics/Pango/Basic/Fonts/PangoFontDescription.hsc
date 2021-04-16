{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts.PangoFontDescription where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Primitive
import Data.Bits
import Data.Bool
import Data.Word
import Data.Int

import Graphics.Pango.Values
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

#include <pango/pango.h>

class PangoFontDescriptionSetting s where
	pangoFontDescriptionSet :: PrimMonad m =>
		PangoFontDescription (PrimState m) -> s -> m ()
	pangoFontDescriptionGetUnsafe :: PrimMonad m =>
		PangoFontDescription (PrimState m) -> m s
	pangoFontDescriptionMaskBit :: PangoFontMask

pangoFontDescriptionGet :: forall s m . (PangoFontDescriptionSetting s, PrimMonad m) =>
	PangoFontDescription (PrimState m) -> m (Maybe s)
pangoFontDescriptionGet fd = do
	PangoFontMask fm <- pangoFontDescriptionGetSetFields fd
	let	PangoFontMask mb = pangoFontDescriptionMaskBit @s
	bool (pure Nothing) (Just <$> pangoFontDescriptionGetUnsafe fd) $ fm .&. mb /= zeroBits

pangoFontDescriptionUnset :: forall s m . (PangoFontDescriptionSetting s, PrimMonad m) =>
	PangoFontDescription (PrimState m) -> m ()
pangoFontDescriptionUnset fd = pangoFontDescriptionUnsetFields fd (pangoFontDescriptionMaskBit @s)

newtype Family = Family String deriving Show

instance PangoFontDescriptionSetting Family where
	pangoFontDescriptionSet fd (Family f) = pangoFontDescriptionSetFamily fd f
	pangoFontDescriptionGetUnsafe fd = Family <$> pangoFontDescriptionGetFamily fd
	pangoFontDescriptionMaskBit = pangoFontMaskFamily

pangoFontDescriptionSetFamily :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> String -> m ()
pangoFontDescriptionSetFamily (PangoFontDescription fpfd) f = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> withCString f \cf ->
		c_pango_font_description_set_family pfd cf

foreign import ccall "pango_font_description_set_family" c_pango_font_description_set_family ::
	Ptr (PangoFontDescription s) -> CString -> IO ()

foreign import ccall "pango_font_description_set_family_static" c_pango_font_description_set_family_static ::
	Ptr (PangoFontDescription s) -> CString -> IO ()

newForeignCString :: String -> IO (ForeignPtr CChar)
newForeignCString s = do
	p <- newCString s
	newForeignPtr p (free p)

pangoFontDescriptionSetFamilyStatic :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> String -> m ()
pangoFontDescriptionSetFamilyStatic (PangoFontDescription fpfd) f = unsafeIOToPrim do
	fcf <- newForeignCString f
	addForeignPtrFinalizer fpfd $ touchForeignPtr fcf
	withForeignPtr fpfd \pfd -> withForeignPtr fcf \cf ->
		c_pango_font_description_set_family_static pfd cf

pangoFontDescriptionGetFamily :: PrimMonad m => PangoFontDescription (PrimState m) -> m String
pangoFontDescriptionGetFamily (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_get_family pfd

foreign import ccall "pango_font_description_get_family" c_pango_font_description_get_family ::
	Ptr (PangoFontDescription s) -> IO CString

instance PangoFontDescriptionSetting PangoStyle where
	pangoFontDescriptionSet = pangoFontDescriptionSetStyle
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetStyle
	pangoFontDescriptionMaskBit = pangoFontMaskStyle

pangoFontDescriptionSetStyle :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoStyle -> m ()
pangoFontDescriptionSetStyle (PangoFontDescription fpfd) (PangoStyle s) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_style pfd s

foreign import ccall "pango_font_description_set_style" c_pango_font_description_set_style ::
	Ptr (PangoFontDescription s) -> #{type PangoStyle} -> IO ()

pangoFontDescriptionGetStyle :: PrimMonad m => PangoFontDescription (PrimState m) -> m PangoStyle
pangoFontDescriptionGetStyle (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> PangoStyle <$> c_pango_font_description_get_style pfd

foreign import ccall "pango_font_description_get_style" c_pango_font_description_get_style ::
	Ptr (PangoFontDescription s) -> IO #type PangoStyle

instance PangoFontDescriptionSetting PangoVariant where
	pangoFontDescriptionSet = pangoFontDescriptionSetVariant
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetVariant
	pangoFontDescriptionMaskBit = pangoFontMaskVariant

pangoFontDescriptionSetVariant :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoVariant -> m ()
pangoFontDescriptionSetVariant (PangoFontDescription fpfd) (PangoVariant pv) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_variant pfd pv

foreign import ccall "pango_font_description_set_variant" c_pango_font_description_set_variant ::
	Ptr (PangoFontDescription s) -> #{type PangoVariant} -> IO ()

pangoFontDescriptionGetVariant :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoVariant
pangoFontDescriptionGetVariant (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		PangoVariant <$> c_pango_font_description_get_variant pfd

foreign import ccall "pango_font_description_get_variant" c_pango_font_description_get_variant ::
	Ptr (PangoFontDescription s) -> IO #type PangoVariant

instance PangoFontDescriptionSetting PangoWeight where
	pangoFontDescriptionSet = pangoFontDescriptionSetWeight
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetWeight
	pangoFontDescriptionMaskBit = pangoFontMaskWeight

pangoFontDescriptionSetWeight :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoWeight -> m ()
pangoFontDescriptionSetWeight (PangoFontDescription fpfd) (PangoWeight wt) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_weight pfd wt

foreign import ccall "pango_font_description_set_weight" c_pango_font_description_set_weight ::
	Ptr (PangoFontDescription s) -> #{type PangoWeight} -> IO ()

pangoFontDescriptionGetWeight :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoWeight
pangoFontDescriptionGetWeight (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		PangoWeight <$> c_pango_font_description_get_weight pfd

foreign import ccall "pango_font_description_get_weight" c_pango_font_description_get_weight ::
	Ptr (PangoFontDescription s) -> IO #type PangoWeight

instance PangoFontDescriptionSetting PangoStretch where
	pangoFontDescriptionSet = pangoFontDescriptionSetStretch
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetStretch
	pangoFontDescriptionMaskBit = pangoFontMaskStretch

pangoFontDescriptionSetStretch :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoStretch -> m ()
pangoFontDescriptionSetStretch (PangoFontDescription fpfd) (PangoStretch ps) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_stretch pfd ps

foreign import ccall "pango_font_description_set_stretch" c_pango_font_description_set_stretch ::
	Ptr (PangoFontDescription s) -> #{type PangoStretch} -> IO ()

pangoFontDescriptionGetStretch :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoStretch
pangoFontDescriptionGetStretch (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> PangoStretch <$> c_pango_font_description_get_stretch pfd

foreign import ccall "pango_font_description_get_stretch" c_pango_font_description_get_stretch ::
	Ptr (PangoFontDescription s) -> IO #type PangoStretch

data Size = Size CDouble | AbsoluteSize CDouble deriving Show

instance PangoFontDescriptionSetting Size where
	pangoFontDescriptionSet fd = \case
		Size s -> pangoFontDescriptionSetSize fd . round $ s * #{const PANGO_SCALE}
		AbsoluteSize as -> pangoFontDescriptionSetAbsoluteSize fd $ as * #{const PANGO_SCALE}
	pangoFontDescriptionGetUnsafe fd = do
		a <- pangoFontDescriptionGetSizeIsAbsolute fd
		s <- pangoFontDescriptionGetSize fd
		pure $ bool (Size $ fromIntegral s / #{const PANGO_SCALE}) (AbsoluteSize $ fromIntegral s / #{const PANGO_SCALE}) a
	pangoFontDescriptionMaskBit = pangoFontMaskSize

pangoFontDescriptionSetSize :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> CInt -> m ()
pangoFontDescriptionSetSize (PangoFontDescription fpfd) n = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_size pfd n

foreign import ccall "pango_font_description_set_size" c_pango_font_description_set_size ::
	Ptr (PangoFontDescription s) -> CInt -> IO ()

pangoFontDescriptionGetSize :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m CInt
pangoFontDescriptionGetSize (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd c_pango_font_description_get_size

foreign import ccall "pango_font_description_get_size" c_pango_font_description_get_size ::
	Ptr (PangoFontDescription s) -> IO CInt

pangoFontDescriptionSetAbsoluteSize :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> CDouble -> m ()
pangoFontDescriptionSetAbsoluteSize (PangoFontDescription fpfd) sz = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_absolute_size pfd sz

foreign import ccall "pango_font_description_set_absolute_size" c_pango_font_description_set_absolute_size ::
	Ptr (PangoFontDescription s) -> CDouble -> IO ()

pangoFontDescriptionGetSizeIsAbsolute :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m Bool
pangoFontDescriptionGetSizeIsAbsolute (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		gbooleanToBool <$> c_pango_font_description_get_size_is_absolute pfd

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean False = #{const FALSE}
boolToGboolean True = #{const TRUE}

foreign import ccall "pango_font_description_get_size_is_absolute"
	c_pango_font_description_get_size_is_absolute ::
	Ptr (PangoFontDescription s) -> IO #type gboolean

instance PangoFontDescriptionSetting PangoGravity where
	pangoFontDescriptionSet = pangoFontDescriptionSetGravity
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetGravity
	pangoFontDescriptionMaskBit = pangoFontMaskGravity

pangoFontDescriptionSetGravity :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoGravity -> m ()
pangoFontDescriptionSetGravity (PangoFontDescription fpfd) (PangoGravity gr) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_gravity pfd gr

foreign import ccall "pango_font_description_set_gravity"
	c_pango_font_description_set_gravity ::
	Ptr (PangoFontDescription s) -> #{type PangoGravity} -> IO ()

pangoFontDescriptionGetGravity :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoGravity
pangoFontDescriptionGetGravity (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		PangoGravity <$> c_pango_font_description_get_gravity pfd

foreign import ccall "pango_font_description_get_gravity"
	c_pango_font_description_get_gravity ::
	Ptr (PangoFontDescription s) -> IO #type PangoGravity

pangoFontDescriptionGetSetFields :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m PangoFontMask
pangoFontDescriptionGetSetFields (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		PangoFontMask <$> c_pango_font_description_get_set_fields pfd

foreign import ccall "pango_font_description_get_set_fields"
	c_pango_font_description_get_set_fields ::
	Ptr (PangoFontDescription s) -> IO #type PangoFontMask

pangoFontDescriptionUnsetFields :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> PangoFontMask -> m ()
pangoFontDescriptionUnsetFields (PangoFontDescription fpfd) (PangoFontMask msk) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_unset_fields pfd msk

foreign import ccall "pango_font_description_unset_fields"
	c_pango_font_description_unset_fields ::
	Ptr (PangoFontDescription s) -> #{type PangoFontMask} -> IO ()

pangoFontDescriptionMerge :: PrimMonad m =>
	PangoFontDescription (PrimState m) ->
	PangoFontDescription (PrimState m) -> Bool -> m ()
pangoFontDescriptionMerge (PangoFontDescription fdsc)
	(PangoFontDescription fdsctm) re = unsafeIOToPrim
	$ withForeignPtr fdsc \pdsc -> withForeignPtr fdsctm \pdsctm ->
		c_pango_font_description_merge pdsc pdsctm $ boolToGboolean re

foreign import ccall "pango_font_description_merge"
	c_pango_font_description_merge ::
	Ptr (PangoFontDescription s) -> Ptr (PangoFontDescription s) -> #{type gboolean} -> IO ()

pangoFontDescriptionBetterMatch :: PrimMonad m =>
	PangoFontDescription (PrimState m) ->
	Maybe (PangoFontDescription (PrimState m)) ->
	PangoFontDescription (PrimState m) -> m Bool
pangoFontDescriptionBetterMatch
	(PangoFontDescription fdsc) mom (PangoFontDescription fnm) = unsafeIOToPrim
	$ gbooleanToBool <$> withForeignPtr fdsc \pdsc -> withMaybePangoFontDescription mom \pom -> withForeignPtr fnm \pnm ->
		c_pango_font_description_better_match pdsc pom pnm

withMaybePangoFontDescription ::
	Maybe (PangoFontDescription s) -> (Ptr (PangoFontDescription s) -> IO a) -> IO a
withMaybePangoFontDescription = \case
	Nothing -> ($ nullPtr)
	Just (PangoFontDescription ffd) -> withForeignPtr ffd

foreign import ccall "pango_font_description_better_match"
	c_pango_font_description_better_match ::
	Ptr (PangoFontDescription s) -> Ptr (PangoFontDescription s) ->
	Ptr (PangoFontDescription s) -> IO #{type gboolean}

pangoFontDescriptionToString :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m String
pangoFontDescriptionToString (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_string pfd

foreign import ccall "pango_font_description_to_string"
	c_pango_font_description_to_string ::
	Ptr (PangoFontDescription s) -> IO CString

pangoFontDescriptionToFilename :: PrimMonad m =>
	PangoFontDescription (PrimState m) -> m String
pangoFontDescriptionToFilename (PangoFontDescription fpfd) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_filename pfd

foreign import ccall "pango_font_description_to_filename"
	c_pango_font_description_to_filename ::
	Ptr (PangoFontDescription s) -> IO CString
