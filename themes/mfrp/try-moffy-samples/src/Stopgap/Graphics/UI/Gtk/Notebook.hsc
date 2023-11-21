{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Notebook where

import Foreign.Ptr
import Data.Int
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget

data NTag

newtype N = N (Ptr NTag) deriving Show

instance IsPtr N where type Tag N = NTag; fromPtr = N; toPtr (N p) = p
instance Gtk.Widget.IsW N where toW (N p) = Gtk.Widget.W $ castPtr p

new :: IO N
new = N <$> c_gtk_notebook_new

foreign import ccall "gtk_notebook_new" c_gtk_notebook_new :: IO (Ptr NTag)

appendPage :: (Gtk.Widget.IsW c, Gtk.Widget.IsW l) =>
	N -> c -> l -> IO #{type int}
appendPage (N n)
	(Gtk.Widget.toW -> Gtk.Widget.W c) (Gtk.Widget.toW -> Gtk.Widget.W l) =
	c_gtk_notebook_append_page n c l

foreign import ccall "gtk_notebook_append_page" c_gtk_notebook_append_page ::
	Ptr NTag -> Ptr Gtk.Widget.WTag -> Ptr Gtk.Widget.WTag -> IO #{type int}

getNPages :: N -> IO #{type int}
getNPages (N n) = c_gtk_notebook_get_n_pages n

foreign import ccall "gtk_notebook_get_n_pages" c_gtk_notebook_get_n_pages ::
	Ptr NTag -> IO #{type int}
