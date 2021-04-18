{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Control.Monad
import Control.Monad.Primitive

import Graphics.Pango.Types

newtype PangoLayoutPrim = PangoLayoutPrim (ForeignPtr PangoLayoutPrim) deriving Show

mkPangoLayoutPrim :: Ptr PangoLayoutPrim -> IO PangoLayoutPrim
mkPangoLayoutPrim p = PangoLayoutPrim <$> newForeignPtr p (c_g_object_unref p)
