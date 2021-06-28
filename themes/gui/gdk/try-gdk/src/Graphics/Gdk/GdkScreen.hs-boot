{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkScreen where

import Foreign.Ptr

newtype GdkScreen = GdkScreen (Ptr GdkScreen)
