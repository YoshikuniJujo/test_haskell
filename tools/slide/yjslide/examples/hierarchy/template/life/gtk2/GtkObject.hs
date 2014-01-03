{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkObject (
	gtkWidgetShow,
	gtkWindowNew
) where

import Control.Applicative
import Foreign.Ptr
import Foreign.C.Types

import GObject

gClass "GObject" "GtkObject"

gClass "GtkObject" "GtkWidget"
foreign import ccall "gtk/gtk.h gtk_widget_show" c_gtkWidgetShow ::
	Ptr GtkWidget -> IO ()
gtkWidgetShow :: GtkWidget -> IO ()
gtkWidgetShow = c_gtkWidgetShow . pointer

gClass "GtkWidget" "GtkContainer"

gClass "GtkContainer" "GtkBin"

gClass "GtkBin" "GtkWindow"
foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)
gtkWindowNew :: IO SomeGtkWindow
gtkWindowNew = SomeGtkWindow <$> c_gtkWindowNew 0
