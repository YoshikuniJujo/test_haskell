{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.ApplicationWindow where

import Foreign.Ptr

import Stopgap.Graphics.UI.Gtk.Window qualified as Gtk.Window
import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application

data ATag

data A = A (Ptr ATag) deriving Show

instance Gtk.Window.IsW A where toW = window

new :: Gtk.Application.A -> IO A
new (Gtk.Application.A a) = A <$> c_gtk_application_window_new a

foreign import ccall "gtk_application_window_new"
	c_gtk_application_window_new ::
	Ptr Gtk.Application.ATag -> IO (Ptr ATag)

window :: A -> Gtk.Window.W
window (A a) = Gtk.Window.W $ c_GTK_WINDOW a

foreign import capi "gtk/gtk.h GTK_WINDOW" c_GTK_WINDOW ::
	Ptr ATag -> Ptr Gtk.Window.WTag
