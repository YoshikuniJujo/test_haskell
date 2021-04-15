{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts.PangoFontDescription.Type where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Control.Monad.Primitive

newtype PangoFontDescription s =
	PangoFontDescription (ForeignPtr (PangoFontDescription s)) deriving Show

pangoFontDescriptionNew :: PrimMonad m => m (PangoFontDescription (PrimState m))
pangoFontDescriptionNew = unsafeIOToPrim
	$ mkPangoFontDescription =<< c_pango_font_description_new

foreign import ccall "pango_font_description_new"
	c_pango_font_description_new :: IO (Ptr (PangoFontDescription s))

mkPangoFontDescription ::
	Ptr (PangoFontDescription s) -> IO (PangoFontDescription s)
mkPangoFontDescription p = PangoFontDescription
	<$> newForeignPtr p (c_pango_font_description_free p)

foreign import ccall "pango_font_description_free"
	c_pango_font_description_free :: Ptr (PangoFontDescription s) -> IO ()
