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

import System.IO.Unsafe

#include <pango/pango.h>

class PangoFontDescriptionSetting s where
	pangoFontDescriptionSet :: PrimMonad m =>
		PangoFontDescriptionPrim (PrimState m) -> s -> m ()
	pangoFontDescriptionGetUnsafe :: PangoFontDescription -> s
	pangoFontDescriptionMaskBit :: PangoFontMask

pangoFontDescriptionGet :: forall s . PangoFontDescriptionSetting s =>
	PangoFontDescription -> Maybe s
pangoFontDescriptionGet = \case
	PangoFontDescriptionNull -> Nothing
	fd -> let
		PangoFontMask fm = pangoFontDescriptionGetSetFields fd
		PangoFontMask mb = pangoFontDescriptionMaskBit @s in
		bool Nothing (Just $ pangoFontDescriptionGetUnsafe fd) $ fm .&. mb /= zeroBits

pangoFontDescriptionUnset :: forall s m . (PangoFontDescriptionSetting s, PrimMonad m) =>
	PangoFontDescriptionPrim (PrimState m) -> m ()
pangoFontDescriptionUnset fd = pangoFontDescriptionUnsetFields fd (pangoFontDescriptionMaskBit @s)

newtype Family = Family String deriving Show

instance PangoFontDescriptionSetting Family where
	pangoFontDescriptionSet fd (Family f) = pangoFontDescriptionSetFamily fd f
	pangoFontDescriptionGetUnsafe fd = Family $ pangoFontDescriptionGetFamily fd
	pangoFontDescriptionMaskBit = pangoFontMaskFamily

pangoFontDescriptionSetFamily :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> String -> m ()
pangoFontDescriptionSetFamily (PangoFontDescriptionPrim fpfd) f = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> withCString f \cf ->
		c_pango_font_description_set_family pfd cf

foreign import ccall "pango_font_description_set_family" c_pango_font_description_set_family ::
	Ptr (PangoFontDescriptionPrim s) -> CString -> IO ()

foreign import ccall "pango_font_description_set_family_static" c_pango_font_description_set_family_static ::
	Ptr (PangoFontDescriptionPrim s) -> CString -> IO ()

newForeignCString :: String -> IO (ForeignPtr CChar)
newForeignCString s = do
	p <- newCString s
	newForeignPtr p (free p)

pangoFontDescriptionSetFamilyStatic :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> String -> m ()
pangoFontDescriptionSetFamilyStatic (PangoFontDescriptionPrim fpfd) f = unsafeIOToPrim do
	fcf <- newForeignCString f
	addForeignPtrFinalizer fpfd $ touchForeignPtr fcf
	withForeignPtr fpfd \pfd -> withForeignPtr fcf \cf ->
		c_pango_font_description_set_family_static pfd cf

pangoFontDescriptionGetFamily :: PangoFontDescription  -> String
pangoFontDescriptionGetFamily PangoFontDescriptionNull = error "PangoFontDescriptionNull"
pangoFontDescriptionGetFamily (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_get_family pfd

foreign import ccall "pango_font_description_get_family" c_pango_font_description_get_family ::
	Ptr PangoFontDescription -> IO CString

instance PangoFontDescriptionSetting PangoStyle where
	pangoFontDescriptionSet = pangoFontDescriptionSetStyle
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetStyle
	pangoFontDescriptionMaskBit = pangoFontMaskStyle

pangoFontDescriptionSetStyle :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoStyle -> m ()
pangoFontDescriptionSetStyle (PangoFontDescriptionPrim fpfd) (PangoStyle s) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_style pfd s

foreign import ccall "pango_font_description_set_style" c_pango_font_description_set_style ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoStyle} -> IO ()

pangoFontDescriptionGetStyle :: PangoFontDescription -> PangoStyle
pangoFontDescriptionGetStyle PangoFontDescriptionNull = error "PangoFontDescriptionNull"
pangoFontDescriptionGetStyle (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd -> PangoStyle <$> c_pango_font_description_get_style pfd

foreign import ccall "pango_font_description_get_style" c_pango_font_description_get_style ::
	Ptr PangoFontDescription -> IO #type PangoStyle

instance PangoFontDescriptionSetting PangoVariant where
	pangoFontDescriptionSet = pangoFontDescriptionSetVariant
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetVariant
	pangoFontDescriptionMaskBit = pangoFontMaskVariant

pangoFontDescriptionSetVariant :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoVariant -> m ()
pangoFontDescriptionSetVariant (PangoFontDescriptionPrim fpfd) (PangoVariant pv) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_variant pfd pv

foreign import ccall "pango_font_description_set_variant" c_pango_font_description_set_variant ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoVariant} -> IO ()

pangoFontDescriptionGetVariant :: PangoFontDescription -> PangoVariant
pangoFontDescriptionGetVariant PangoFontDescriptionNull = error "PangoFontDescriptionNull"
pangoFontDescriptionGetVariant (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoVariant <$> c_pango_font_description_get_variant pfd

foreign import ccall "pango_font_description_get_variant" c_pango_font_description_get_variant ::
	Ptr PangoFontDescription -> IO #type PangoVariant

instance PangoFontDescriptionSetting PangoWeight where
	pangoFontDescriptionSet = pangoFontDescriptionSetWeight
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetWeight
	pangoFontDescriptionMaskBit = pangoFontMaskWeight

pangoFontDescriptionSetWeight :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoWeight -> m ()
pangoFontDescriptionSetWeight (PangoFontDescriptionPrim fpfd) (PangoWeight wt) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_weight pfd wt

foreign import ccall "pango_font_description_set_weight" c_pango_font_description_set_weight ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoWeight} -> IO ()

pangoFontDescriptionGetWeight :: PangoFontDescription -> PangoWeight
pangoFontDescriptionGetWeight PangoFontDescriptionNull = error "PangoFontDescriptionNull"
pangoFontDescriptionGetWeight (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoWeight <$> c_pango_font_description_get_weight pfd

foreign import ccall "pango_font_description_get_weight" c_pango_font_description_get_weight ::
	Ptr PangoFontDescription -> IO #type PangoWeight

instance PangoFontDescriptionSetting PangoStretch where
	pangoFontDescriptionSet = pangoFontDescriptionSetStretch
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetStretch
	pangoFontDescriptionMaskBit = pangoFontMaskStretch

pangoFontDescriptionSetStretch :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoStretch -> m ()
pangoFontDescriptionSetStretch (PangoFontDescriptionPrim fpfd) (PangoStretch ps) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd -> c_pango_font_description_set_stretch pfd ps

foreign import ccall "pango_font_description_set_stretch" c_pango_font_description_set_stretch ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoStretch} -> IO ()

pangoFontDescriptionGetStretch :: PangoFontDescription -> PangoStretch
pangoFontDescriptionGetStretch PangoFontDescriptionNull = error "PangoFontDescriptionNull"
pangoFontDescriptionGetStretch (PangoFontDescription fpfd) = unsafePerformIO
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
	pangoFontDescriptionMaskBit = pangoFontMaskSize

pangoFontDescriptionSetSize :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> CInt -> m ()
pangoFontDescriptionSetSize (PangoFontDescriptionPrim fpfd) n = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_size pfd n

foreign import ccall "pango_font_description_set_size" c_pango_font_description_set_size ::
	Ptr (PangoFontDescriptionPrim s) -> CInt -> IO ()

pangoFontDescriptionGetSize :: PangoFontDescription -> CInt
pangoFontDescriptionGetSize PangoFontDescriptionNull = error "PangoFontDescriptionNull"
pangoFontDescriptionGetSize (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd c_pango_font_description_get_size

foreign import ccall "pango_font_description_get_size" c_pango_font_description_get_size ::
	Ptr PangoFontDescription -> IO CInt

pangoFontDescriptionSetAbsoluteSize :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> CDouble -> m ()
pangoFontDescriptionSetAbsoluteSize (PangoFontDescriptionPrim fpfd) sz = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_absolute_size pfd sz

foreign import ccall "pango_font_description_set_absolute_size" c_pango_font_description_set_absolute_size ::
	Ptr (PangoFontDescriptionPrim s) -> CDouble -> IO ()

pangoFontDescriptionGetSizeIsAbsolute :: PangoFontDescription -> Bool
pangoFontDescriptionGetSizeIsAbsolute PangoFontDescriptionNull = error "PangoFontDescriptionNull"
pangoFontDescriptionGetSizeIsAbsolute (PangoFontDescription fpfd) = unsafePerformIO
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
	Ptr PangoFontDescription -> IO #type gboolean

instance PangoFontDescriptionSetting PangoGravity where
	pangoFontDescriptionSet = pangoFontDescriptionSetGravity
	pangoFontDescriptionGetUnsafe = pangoFontDescriptionGetGravity
	pangoFontDescriptionMaskBit = pangoFontMaskGravity

pangoFontDescriptionSetGravity :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoGravity -> m ()
pangoFontDescriptionSetGravity (PangoFontDescriptionPrim fpfd) (PangoGravity gr) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_set_gravity pfd gr

foreign import ccall "pango_font_description_set_gravity"
	c_pango_font_description_set_gravity ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoGravity} -> IO ()

pangoFontDescriptionGetGravity :: PangoFontDescription -> PangoGravity
pangoFontDescriptionGetGravity PangoFontDescriptionNull = error "PangoFontDescriptionNull"
pangoFontDescriptionGetGravity (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoGravity <$> c_pango_font_description_get_gravity pfd

foreign import ccall "pango_font_description_get_gravity"
	c_pango_font_description_get_gravity ::
	Ptr PangoFontDescription -> IO #type PangoGravity

pangoFontDescriptionGetSetFields :: PangoFontDescription -> PangoFontMask
pangoFontDescriptionGetSetFields PangoFontDescriptionNull = PangoFontMask 0
pangoFontDescriptionGetSetFields (PangoFontDescription fpfd) = unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		PangoFontMask <$> c_pango_font_description_get_set_fields pfd

foreign import ccall "pango_font_description_get_set_fields"
	c_pango_font_description_get_set_fields ::
	Ptr PangoFontDescription -> IO #type PangoFontMask

pangoFontDescriptionUnsetFields :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> PangoFontMask -> m ()
pangoFontDescriptionUnsetFields (PangoFontDescriptionPrim fpfd) (PangoFontMask msk) = unsafeIOToPrim
	$ withForeignPtr fpfd \pfd ->
		c_pango_font_description_unset_fields pfd msk

foreign import ccall "pango_font_description_unset_fields"
	c_pango_font_description_unset_fields ::
	Ptr (PangoFontDescriptionPrim s) -> #{type PangoFontMask} -> IO ()

pangoFontDescriptionMerge :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) ->
	PangoFontDescriptionPrim (PrimState m) -> Bool -> m ()
pangoFontDescriptionMerge (PangoFontDescriptionPrim fdsc)
	(PangoFontDescriptionPrim fdsctm) re = unsafeIOToPrim
	$ withForeignPtr fdsc \pdsc -> withForeignPtr fdsctm \pdsctm ->
		c_pango_font_description_merge pdsc pdsctm $ boolToGboolean re

foreign import ccall "pango_font_description_merge"
	c_pango_font_description_merge ::
	Ptr (PangoFontDescriptionPrim s) -> Ptr (PangoFontDescriptionPrim s) -> #{type gboolean} -> IO ()

pangoFontDescriptionBetterMatch :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) ->
	Maybe (PangoFontDescriptionPrim (PrimState m)) ->
	PangoFontDescriptionPrim (PrimState m) -> m Bool
pangoFontDescriptionBetterMatch
	(PangoFontDescriptionPrim fdsc) mom (PangoFontDescriptionPrim fnm) = unsafeIOToPrim
	$ gbooleanToBool <$> withForeignPtr fdsc \pdsc -> withMaybePangoFontDescriptionPrim mom \pom -> withForeignPtr fnm \pnm ->
		c_pango_font_description_better_match pdsc pom pnm

withMaybePangoFontDescriptionPrim ::
	Maybe (PangoFontDescriptionPrim s) -> (Ptr (PangoFontDescriptionPrim s) -> IO a) -> IO a
withMaybePangoFontDescriptionPrim = \case
	Nothing -> ($ nullPtr)
	Just (PangoFontDescriptionPrim ffd) -> withForeignPtr ffd

foreign import ccall "pango_font_description_better_match"
	c_pango_font_description_better_match ::
	Ptr (PangoFontDescriptionPrim s) -> Ptr (PangoFontDescriptionPrim s) ->
	Ptr (PangoFontDescriptionPrim s) -> IO #{type gboolean}

pangoFontDescriptionToString :: PangoFontDescription -> Maybe String
pangoFontDescriptionToString PangoFontDescriptionNull = Nothing
pangoFontDescriptionToString (PangoFontDescription fpfd) = Just . unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_string pfd

foreign import ccall "pango_font_description_to_string"
	c_pango_font_description_to_string ::
	Ptr PangoFontDescription -> IO CString

pangoFontDescriptionToFilename :: PangoFontDescription -> Maybe String
pangoFontDescriptionToFilename PangoFontDescriptionNull = Nothing
pangoFontDescriptionToFilename (PangoFontDescription fpfd) = Just . unsafePerformIO
	$ withForeignPtr fpfd \pfd ->
		peekCString =<< c_pango_font_description_to_filename pfd

foreign import ccall "pango_font_description_to_filename"
	c_pango_font_description_to_filename ::
	Ptr PangoFontDescription -> IO CString
