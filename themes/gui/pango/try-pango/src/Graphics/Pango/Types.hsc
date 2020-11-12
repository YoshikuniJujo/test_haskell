{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Control.Monad.ST

newtype PangoLayoutIo = PangoLayoutIo (ForeignPtr PangoLayoutIo) deriving Show

makePangoLayoutIo :: Ptr PangoLayoutIo -> IO PangoLayoutIo
makePangoLayoutIo p = PangoLayoutIo <$> newForeignPtr p (c_g_object_unref p)

newtype PangoLayout = PangoLayout (ForeignPtr PangoLayout) deriving Show

makePangoLayout :: Ptr PangoLayout -> IO PangoLayout
makePangoLayout p = PangoLayout <$> newForeignPtr p (c_g_object_unref p)

foreign import ccall "pango_layout_copy" c_pango_layout_freeze ::
	Ptr PangoLayoutIo -> IO (Ptr PangoLayout)

pangoLayoutFreeze :: PangoLayoutIo -> IO PangoLayout
pangoLayoutFreeze (PangoLayoutIo fpl) =
	withForeignPtr fpl \pl -> makePangoLayout =<< c_pango_layout_freeze pl

foreign import ccall "pango_layout_copy" c_pango_layout_thaw ::
	Ptr PangoLayout -> IO (Ptr PangoLayoutIo)

pangoLayoutThaw :: PangoLayout -> IO PangoLayoutIo
pangoLayoutThaw (PangoLayout fpl) =
	withForeignPtr fpl \pl -> makePangoLayoutIo =<< c_pango_layout_thaw pl

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newtype PangoFontDescriptionPrim s = PangoFontDescriptionPrim (ForeignPtr (PangoFontDescriptionPrim s)) deriving Show

makePangoFontDescriptionPrim :: Ptr (PangoFontDescriptionPrim s) -> IO (PangoFontDescriptionPrim s)
makePangoFontDescriptionPrim p = PangoFontDescriptionPrim <$> newForeignPtr p (c_pango_font_description_prim_free p)

newtype PangoFontDescription = PangoFontDescription (ForeignPtr PangoFontDescription) deriving Show

makePangoFontDescription :: Ptr PangoFontDescription -> IO PangoFontDescription
makePangoFontDescription p = PangoFontDescription <$> newForeignPtr p (c_pango_font_description_free p)

newtype PangoFontDescriptionOld = PangoFontDescriptionOld (ForeignPtr PangoFontDescriptionOld) deriving Show

makePangoFontDescriptionOld :: Ptr PangoFontDescriptionOld -> IO PangoFontDescriptionOld
makePangoFontDescriptionOld p = PangoFontDescriptionOld <$> newForeignPtr p (c_pango_font_description_old_free p)

pangoFontDescriptionPrimToOld :: PangoFontDescriptionPrim RealWorld -> PangoFontDescriptionOld
pangoFontDescriptionPrimToOld (PangoFontDescriptionPrim fpfd) = PangoFontDescriptionOld $ castForeignPtr fpfd

foreign import ccall "pango_font_description_free" c_pango_font_description_prim_free ::
	Ptr (PangoFontDescriptionPrim s) -> IO ()

foreign import ccall "pango_font_description_free" c_pango_font_description_free ::
	Ptr PangoFontDescription -> IO ()

foreign import ccall "pango_font_description_free" c_pango_font_description_old_free ::
	Ptr PangoFontDescriptionOld -> IO ()

newtype PangoContext = PangoContext (ForeignPtr PangoContext) deriving Show

makePangoContext :: Ptr PangoContext -> IO PangoContext
makePangoContext p = PangoContext <$> newForeignPtr p (c_g_object_unref p)
