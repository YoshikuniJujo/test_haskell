{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows where

import Foreign.Ptr

newtype GdkWindow = GdkWindow (Ptr GdkWindow)
