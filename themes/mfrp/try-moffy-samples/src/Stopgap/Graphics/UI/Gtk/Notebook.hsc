{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Notebook where

import Foreign.Ptr
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget

data NTag

newtype N = N (Ptr NTag) deriving Show

instance IsPtr N where type Tag N = NTag; fromPtr = N; toPtr (N p) = p
instance Gtk.Widget.IsW N where toW (N p) = Gtk.Widget.W $ castPtr p

new :: IO N
new = N <$> c_gtk_notebook_new

foreign import ccall "gtk_notebook_new" c_gtk_notebook_new :: IO (Ptr NTag)
