{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Container where

import Foreign.Ptr
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget

data CTag

newtype C = C (Ptr CTag) deriving Show

class Gtk.Widget.IsW c => IsC c where toC :: c -> C

add :: (IsC c, Gtk.Widget.IsW w) => c -> w -> IO ()
add c w = c_gtk_container_add (toC c) (Gtk.Widget.toW w)

foreign import ccall "gtk_container_add" c_gtk_container_add ::
	C -> Gtk.Widget.W -> IO ()
