{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice.GdkAxes where

import Foreign.ForeignPtr
import Foreign.C.Types

newtype Axes = Axes (ForeignPtr CDouble) deriving Show
