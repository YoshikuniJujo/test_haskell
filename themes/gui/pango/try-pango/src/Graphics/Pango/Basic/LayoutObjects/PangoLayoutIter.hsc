{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

newtype PangoLayoutIter s = PangoLayoutIter (ForeignPtr (PangoLayoutIter s)) deriving Show

makePangoLayoutIter :: Ptr (PangoLayoutIter s) -> IO (PangoLayoutIter s)
makePangoLayoutIter p = PangoLayoutIter <$> newForeignPtr p (c_pango_layout_iter_free p)

foreign import ccall "pango_layout_iter_free" c_pango_layout_iter_free ::
	Ptr (PangoLayoutIter s) -> IO ()
