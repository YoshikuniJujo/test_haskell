{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Types where

import Foreign.Ptr

newtype GdkWindow = GdkWindow (Ptr GdkWindow) deriving Show
newtype GdkWindowAttr = GdkWindowAttr (Ptr GdkWindowAttr) deriving Show
