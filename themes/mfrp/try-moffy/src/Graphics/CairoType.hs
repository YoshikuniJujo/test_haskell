{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.CairoType where

import Foreign.Ptr

newtype CairoT = CairoT (Ptr CairoT) deriving Show

newtype CairoSurfaceT = CairoSurfaceT (Ptr CairoSurfaceT) deriving Show
