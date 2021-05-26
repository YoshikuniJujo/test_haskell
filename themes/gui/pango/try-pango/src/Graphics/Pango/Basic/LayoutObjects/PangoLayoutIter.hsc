{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

import Graphics.Pango.Basic.LayoutObjects.PangoLayout

newtype PangoLayoutIter = PangoLayoutIter (ForeignPtr PangoLayoutIter) deriving Show

makePangoLayoutIter :: Ptr PangoLayoutIter -> IO PangoLayoutIter
makePangoLayoutIter p = PangoLayoutIter <$> newForeignPtr p (c_pango_layout_iter_free p)

foreign import ccall "pango_layout_iter_free" c_pango_layout_iter_free ::
	Ptr PangoLayoutIter -> IO ()

pangoLayoutGetIter :: PangoLayout -> IO PangoLayoutIter
pangoLayoutGetIter (PangoLayout_ fl) =
	makePangoLayoutIter =<< withForeignPtr fl c_pango_layout_get_iter

foreign import ccall "pango_layout_get_iter" c_pango_layout_get_iter ::
	Ptr PangoLayout -> IO (Ptr PangoLayoutIter)
