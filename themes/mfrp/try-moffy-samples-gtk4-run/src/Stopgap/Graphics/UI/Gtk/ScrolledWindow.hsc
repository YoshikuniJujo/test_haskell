{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.ScrolledWindow where

import Foreign.Ptr
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.System.GLib.Object qualified as G.Object

data STag

newtype S = S (Ptr STag) deriving Show

instance IsPtr S where type Tag S = STag; fromPtr = S; toPtr (S p) = p
instance G.Object.IsO S where toO (S p) = G.Object.O $ castPtr p
instance Gtk.Widget.IsW S where toW (S p) = Gtk.Widget.W $ castPtr p

new :: IO S
new = S <$> c_gtk_scrolled_window_new

foreign import ccall "gtk_scrolled_window_new" c_gtk_scrolled_window_new ::
	IO (Ptr STag)

setChild :: Gtk.Widget.IsW w => S -> w -> IO ()
setChild (S s) (Gtk.Widget.toW -> Gtk.Widget.W w) =
	c_gtk_scrolled_window_set_child s w

foreign import ccall "gtk_scrolled_window_set_child"
	c_gtk_scrolled_window_set_child ::
	Ptr STag -> Ptr Gtk.Widget.WTag -> IO ()
