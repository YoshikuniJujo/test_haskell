{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows where

import Foreign.Ptr
import Foreign.Storable

newtype GdkWindow = GdkWindow (Ptr GdkWindow)

instance Show GdkWindow
instance Storable GdkWindow
