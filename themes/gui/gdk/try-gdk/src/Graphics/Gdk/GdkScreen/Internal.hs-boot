{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkScreen.Internal where

import Foreign.Ptr

newtype GdkScreen = GdkScreen (Ptr GdkScreen)
