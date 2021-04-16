{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Rendering where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

import Graphics.Pango.Types (c_g_object_unref)

newtype PangoContext = PangoContext (ForeignPtr PangoContext) deriving Show

mkPangoContext :: Ptr PangoContext -> IO PangoContext
mkPangoContext p = PangoContext <$> newForeignPtr p (c_g_object_unref p)
