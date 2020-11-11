{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

newtype PangoLayoutIo = PangoLayoutIo (ForeignPtr PangoLayoutIo) deriving Show

makePangoLayoutIo :: Ptr PangoLayoutIo -> IO PangoLayoutIo
makePangoLayoutIo p = PangoLayoutIo <$> newForeignPtr p (c_g_object_unref p)

newtype PangoLayoutOld = PangoLayoutOld (ForeignPtr PangoLayoutOld) deriving Show

makePangoLayoutOld :: Ptr PangoLayoutOld -> IO PangoLayoutOld
makePangoLayoutOld p = PangoLayoutOld <$> newForeignPtr p (c_g_object_unref p)

pangoLayoutIoToOld :: PangoLayoutIo -> PangoLayoutOld
pangoLayoutIoToOld (PangoLayoutIo fp) = PangoLayoutOld $ castForeignPtr fp

pangoLayoutOldToIo :: PangoLayoutOld -> PangoLayoutIo
pangoLayoutOldToIo (PangoLayoutOld fp) = PangoLayoutIo $ castForeignPtr fp

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newtype PangoFontDescription = PangoFontDescription (ForeignPtr PangoFontDescription) deriving Show

makePangoFontDescription :: Ptr PangoFontDescription -> IO PangoFontDescription
makePangoFontDescription p = PangoFontDescription <$> newForeignPtr p (c_pango_font_description_free p)

foreign import ccall "pango_font_description_free" c_pango_font_description_free ::
	Ptr PangoFontDescription -> IO ()

newtype PangoContext = PangoContext (ForeignPtr PangoContext) deriving Show

makePangoContext :: Ptr PangoContext -> IO PangoContext
makePangoContext p = PangoContext <$> newForeignPtr p (c_g_object_unref p)
