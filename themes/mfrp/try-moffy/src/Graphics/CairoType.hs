{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.CairoType where

import Foreign.Ptr
import Foreign.Tools

newtype CairoT = CairoT (Ptr CairoT) deriving Show

newtype CairoSurfaceT = CairoSurfaceT (PtrForeignPtr CairoSurfaceT) deriving Show
