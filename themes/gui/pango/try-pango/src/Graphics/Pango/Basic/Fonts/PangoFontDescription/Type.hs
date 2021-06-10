{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts.PangoFontDescription.Type (
	-- * PANGO FONT DESCRIPTION
	PangoFontDescription(..), mkPangoFontDescription,

	-- * PANGO FONT DESCRIPTION NULLABLE
	PangoFontDescriptionNullable(..),
	mkPangoFontDescriptionNullable,
	pangoFontDescriptionFromNullable, pangoFontDescriptionToNullable,

	-- * PANGO FONT DESCRIPTION FOR PRIMITIVE MONAD
	PangoFontDescriptionPrim(..),
	mkPangoFontDescriptionPrim,
	PangoFontDescriptionST, PangoFontDescriptionIO,
	pangoFontDescriptionFreeze, pangoFontDescriptionThaw,
	pangoFontDescriptionCopy
	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.C.Struct

data PangoFontDescription
	= PangoFontDescription_ (ForeignPtr PangoFontDescription)
	deriving Show

mkPangoFontDescription :: Ptr PangoFontDescription -> IO PangoFontDescription
mkPangoFontDescription = \case
	NullPtr -> error "bad"
	p -> PangoFontDescription_
		<$> newForeignPtr p (c_pango_font_description_free p)

data PangoFontDescriptionNullable
	= PangoFontDescriptionNull
	| PangoFontDescriptionNotNull (ForeignPtr PangoFontDescription)
	deriving Show

mkPangoFontDescriptionNullable :: Ptr PangoFontDescription -> IO PangoFontDescriptionNullable
mkPangoFontDescriptionNullable = \case
	NullPtr -> pure PangoFontDescriptionNull
	p -> PangoFontDescriptionNotNull
		<$> newForeignPtr p (c_pango_font_description_free p)

pangoFontDescriptionToNullable :: Maybe PangoFontDescription -> PangoFontDescriptionNullable
pangoFontDescriptionToNullable Nothing = PangoFontDescriptionNull
pangoFontDescriptionToNullable (Just (PangoFontDescription_ f)) =
	PangoFontDescriptionNotNull f

pangoFontDescriptionFromNullable :: PangoFontDescriptionNullable -> Maybe PangoFontDescription
pangoFontDescriptionFromNullable PangoFontDescriptionNull = Nothing
pangoFontDescriptionFromNullable (PangoFontDescriptionNotNull f) =
	Just $ PangoFontDescription_ f

foreign import ccall "pango_font_description_copy"
	c_pango_font_description_copy ::
	Ptr PangoFontDescription -> IO (Ptr PangoFontDescription)

foreign import ccall "pango_font_description_free"
	c_pango_font_description_free :: Ptr PangoFontDescription -> IO ()

structPrim "PangoFontDescription"
	'c_pango_font_description_copy 'c_pango_font_description_free [''Show]

mkPangoFontDescriptionPrim ::
	Ptr PangoFontDescription -> IO (PangoFontDescriptionPrim s)
mkPangoFontDescriptionPrim p = PangoFontDescriptionPrim
	<$> newForeignPtr p (c_pango_font_description_free p)
