{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

newtype PangoLayout = PangoLayout (ForeignPtr PangoLayout) deriving Show

makePangoLayout :: Ptr PangoLayout -> IO PangoLayout
makePangoLayout p = PangoLayout <$> newForeignPtr p (c_g_object_unref p)

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newtype PangoFontDescription = PangoFontDescription (ForeignPtr PangoFontDescription) deriving Show

makePangoFontDescription :: Ptr PangoFontDescription -> IO PangoFontDescription
makePangoFontDescription p = PangoFontDescription <$> newForeignPtr p (c_pango_font_description_free p)

foreign import ccall "pango_font_description_free" c_pango_font_description_free ::
	Ptr PangoFontDescription -> IO ()
