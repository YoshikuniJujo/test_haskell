{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDisplay.Internal (GdkDisplay(..)) where

import Foreign.Ptr

newtype GdkDisplay = GdkDisplay (Ptr GdkDisplay)
