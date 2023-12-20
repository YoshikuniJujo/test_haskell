{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.DrawingArea where

import Foreign.Ptr
import Stopgap.Data.Ptr
import Stopgap.System.GLib.Object qualified as G.Object
import Stopgap.Graphics.UI.Gtk.Widget qualified as Widget

data DTag

newtype D = D (Ptr DTag) deriving Show

instance IsPtr D where type Tag D = DTag; toPtr (D p) = p; fromPtr = D
instance G.Object.IsO D where toO (D p) = G.Object.O $ castPtr p
instance Widget.IsW D where toW (D p) = Widget.W $ castPtr p

new :: IO D
new = c_gtk_drawing_area_new

foreign import ccall "gtk_drawing_area_new" c_gtk_drawing_area_new :: IO D
