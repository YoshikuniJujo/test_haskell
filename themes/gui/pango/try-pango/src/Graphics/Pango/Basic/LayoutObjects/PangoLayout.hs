{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayout where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

import Graphics.Pango.Types

newtype PangoLayout = PangoLayout (ForeignPtr PangoLayout) deriving Show

mkPangoLayout :: Ptr PangoLayout -> IO PangoLayout
mkPangoLayout p = PangoLayout <$> newForeignPtr p (c_g_object_unref p)
