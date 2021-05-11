{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.GlyphStorage.PangoMatrix where

import Foreign.ForeignPtr

newtype PangoMatrix = PangoMatrix_ (ForeignPtr PangoMatrix) deriving Show
