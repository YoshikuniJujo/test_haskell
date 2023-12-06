{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.ApplicationWindow where

import Foreign.Ptr
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.System.GLib.Object qualified as G.Object

data ATag

newtype A = A (Ptr ATag) deriving Show

instance IsPtr A where type Tag A = ATag; fromPtr = A; toPtr (A p) = p
instance G.Object.IsO A where toO (A a) = G.Object.O $ castPtr a
instance Gtk.Widget.IsW A where toW (A a) = Gtk.Widget.W $ castPtr a
instance Gtk.Window.IsW A where toW = window

new :: Gtk.Application.A s -> IO A
new (Gtk.Application.A a) = A <$> c_gtk_application_window_new a

foreign import ccall "gtk_application_window_new"
	c_gtk_application_window_new ::
	Ptr Gtk.Application.ATag -> IO (Ptr ATag)

window :: A -> Gtk.Window.W
window (A a) = Gtk.Window.W $ c_GTK_WINDOW a

foreign import capi "gtk/gtk.h GTK_WINDOW" c_GTK_WINDOW ::
	Ptr ATag -> Ptr Gtk.Window.WTag
