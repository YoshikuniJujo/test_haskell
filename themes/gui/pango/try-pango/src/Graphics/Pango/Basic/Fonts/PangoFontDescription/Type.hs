{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts.PangoFontDescription.Type where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Control.Monad.Primitive

newtype PangoFontDescriptionPrim s =
	PangoFontDescriptionPrim (ForeignPtr (PangoFontDescriptionPrim s))
	
instance Show (PangoFontDescriptionPrim s) where
	show _ = "PangoFontDescription"

pangoFontDescriptionNew ::
	PrimMonad m => m (PangoFontDescriptionPrim (PrimState m))
pangoFontDescriptionNew = unsafeIOToPrim
	$ mkPangoFontDescriptionPrim =<< c_pango_font_description_new

foreign import ccall "pango_font_description_new"
	c_pango_font_description_new :: IO (Ptr (PangoFontDescriptionPrim s))

mkPangoFontDescriptionPrim ::
	Ptr (PangoFontDescriptionPrim s) -> IO (PangoFontDescriptionPrim s)
mkPangoFontDescriptionPrim p = PangoFontDescriptionPrim
	<$> newForeignPtr p (c_pango_font_description_free_prim p)

foreign import ccall "pango_font_description_free"
	c_pango_font_description_free_prim ::
	Ptr (PangoFontDescriptionPrim s) -> IO ()

data PangoFontDescription
	= PangoFontDescriptionNull
	| PangoFontDescription (ForeignPtr PangoFontDescription)
	deriving Show

mkPangoFontDescription :: Ptr PangoFontDescription -> IO PangoFontDescription
mkPangoFontDescription p
	| p == nullPtr = pure PangoFontDescriptionNull
	| otherwise = PangoFontDescription
		<$> newForeignPtr p (c_pango_font_description_free p)

foreign import ccall "pango_font_description_free"
	c_pango_font_description_free :: Ptr PangoFontDescription -> IO ()

pangoFontDescriptionFreeze :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> m PangoFontDescription
pangoFontDescriptionFreeze (PangoFontDescriptionPrim ffd) =
	unsafeIOToPrim $ mkPangoFontDescription
		=<< withForeignPtr ffd c_pango_font_description_freeze

foreign import ccall "pango_font_description_copy"
	c_pango_font_description_freeze ::
	Ptr (PangoFontDescriptionPrim s) -> IO (Ptr PangoFontDescription)

pangoFontDescriptionThaw :: PrimMonad m =>
	PangoFontDescription -> m (Maybe (PangoFontDescriptionPrim (PrimState m)))
pangoFontDescriptionThaw PangoFontDescriptionNull = pure Nothing
pangoFontDescriptionThaw (PangoFontDescription ffd) =
	unsafeIOToPrim $ (Just <$>) . mkPangoFontDescriptionPrim
		=<< withForeignPtr ffd c_pango_font_description_thaw

foreign import ccall "pango_font_description_copy"
	c_pango_font_description_thaw ::
	Ptr PangoFontDescription -> IO (Ptr (PangoFontDescriptionPrim s))
