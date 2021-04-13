{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutIo where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

import Graphics.Pango.Types

newtype PangoLayoutIo = PangoLayoutIo (ForeignPtr PangoLayoutIo) deriving Show

makePangoLayoutIo :: Ptr PangoLayoutIo -> IO PangoLayoutIo
makePangoLayoutIo p = PangoLayoutIo <$> newForeignPtr p (c_g_object_unref p)

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
