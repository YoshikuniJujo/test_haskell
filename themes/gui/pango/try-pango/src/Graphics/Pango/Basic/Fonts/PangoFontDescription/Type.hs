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
	deriving Show

pangoFontDescriptionNew ::
	PrimMonad m => m (PangoFontDescriptionPrim (PrimState m))
pangoFontDescriptionNew = unsafeIOToPrim
	$ mkPangoFontDescriptionPrim =<< c_pango_font_description_new

foreign import ccall "pango_font_description_new"
	c_pango_font_description_new :: IO (Ptr (PangoFontDescriptionPrim s))

mkPangoFontDescriptionPrim ::
	Ptr (PangoFontDescriptionPrim s) -> IO (PangoFontDescriptionPrim s)
mkPangoFontDescriptionPrim p = PangoFontDescriptionPrim
	<$> newForeignPtr p (c_pango_font_description_free p)

foreign import ccall "pango_font_description_free"
	c_pango_font_description_free :: Ptr (PangoFontDescriptionPrim s) -> IO ()
