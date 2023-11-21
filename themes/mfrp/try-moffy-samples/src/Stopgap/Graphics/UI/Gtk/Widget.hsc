{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Widget where

import Foreign.Ptr
import Stopgap.System.GLib.Object qualified as G.Object

class G.Object.IsO w => IsW w where toW :: w -> W

data WTag

newtype W = W (Ptr WTag) deriving Show
