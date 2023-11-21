{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.NotebookPage where

import Foreign.Ptr
import Stopgap.Data.Ptr

import Stopgap.System.GLib.Object qualified as G.Object

data NTag

newtype N = N (Ptr NTag) deriving Show

instance IsPtr N where type Tag N = NTag; fromPtr = N; toPtr (N p) = p
instance G.Object.IsO N where toO (N n) = G.Object.O $ castPtr n
