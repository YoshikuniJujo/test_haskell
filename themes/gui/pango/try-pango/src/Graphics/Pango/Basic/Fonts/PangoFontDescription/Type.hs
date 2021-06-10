{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts.PangoFontDescription.Type (
	PangoFontDescription(..), mkPangoFontDescription,
	PangoFontDescriptionNullable(..),
	mkPangoFontDescriptionNullable,
	pangoFontDescriptionFromNullable, pangoFontDescriptionToNullable,
	PangoFontDescriptionPrim(..), pangoFontDescriptionPrimNew,
	pangoFontDescriptionFreeze, pangoFontDescriptionThaw
	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Control.Monad.Primitive

data PangoFontDescription
	= PangoFontDescription_ (ForeignPtr PangoFontDescription)
	deriving Show

data PangoFontDescriptionNullable
	= PangoFontDescriptionNull
	| PangoFontDescriptionNotNull (ForeignPtr PangoFontDescription)
	deriving Show

pangoFontDescriptionToNullable :: Maybe PangoFontDescription -> PangoFontDescriptionNullable
pangoFontDescriptionToNullable Nothing = PangoFontDescriptionNull
pangoFontDescriptionToNullable (Just (PangoFontDescription_ f)) =
	PangoFontDescriptionNotNull f

pangoFontDescriptionFromNullable :: PangoFontDescriptionNullable -> Maybe PangoFontDescription
pangoFontDescriptionFromNullable PangoFontDescriptionNull = Nothing
pangoFontDescriptionFromNullable (PangoFontDescriptionNotNull f) =
	Just $ PangoFontDescription_ f

newtype PangoFontDescriptionPrim s =
	PangoFontDescriptionPrim (ForeignPtr PangoFontDescription) deriving Show

pangoFontDescriptionPrimNew ::
	PrimMonad m => m (PangoFontDescriptionPrim (PrimState m))
pangoFontDescriptionPrimNew = unsafeIOToPrim
	$ mkPangoFontDescriptionPrim =<< c_pango_font_description_new

foreign import ccall "pango_font_description_new"
	c_pango_font_description_new :: IO (Ptr PangoFontDescription)

mkPangoFontDescriptionPrim ::
	Ptr PangoFontDescription -> IO (PangoFontDescriptionPrim s)
mkPangoFontDescriptionPrim p = PangoFontDescriptionPrim
	<$> newForeignPtr p (c_pango_font_description_free p)

mkPangoFontDescription :: Ptr PangoFontDescription -> IO PangoFontDescription
mkPangoFontDescription = \case
	NullPtr -> error "bad"
	p -> PangoFontDescription_
		<$> newForeignPtr p (c_pango_font_description_free p)

mkPangoFontDescriptionNullable :: Ptr PangoFontDescription -> IO PangoFontDescriptionNullable
mkPangoFontDescriptionNullable = \case
	NullPtr -> pure PangoFontDescriptionNull
	p -> PangoFontDescriptionNotNull
		<$> newForeignPtr p (c_pango_font_description_free p)

pangoFontDescriptionFreeze :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> m PangoFontDescription
pangoFontDescriptionFreeze (PangoFontDescriptionPrim ffd) =
	unsafeIOToPrim $ mkPangoFontDescription
		=<< withForeignPtr ffd c_pango_font_description_copy

pangoFontDescriptionThaw :: PrimMonad m =>
	PangoFontDescription -> m (Maybe (PangoFontDescriptionPrim (PrimState m)))
pangoFontDescriptionThaw (PangoFontDescription_ ffd) =
	unsafeIOToPrim $ (Just <$>) . mkPangoFontDescriptionPrim
		=<< withForeignPtr ffd c_pango_font_description_copy

foreign import ccall "pango_font_description_copy"
	c_pango_font_description_copy ::
	Ptr PangoFontDescription -> IO (Ptr PangoFontDescription)

foreign import ccall "pango_font_description_free"
	c_pango_font_description_free :: Ptr PangoFontDescription -> IO ()
