{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkWidget (
	gtkWidgetShow,
	gtkWidgetToGObject,
	gtkWidgetFromGObject,
	module GtkObject,
) where

import Foreign.Ptr

import GtkObject

gClass "GtkObject" "GtkWidget"
foreign import ccall "gtk/gtk.h gtk_widget_show" c_gtkWidgetShow ::
	Ptr GtkWidget -> IO ()
gtkWidgetShow :: GtkWidget -> IO ()
gtkWidgetShow = c_gtkWidgetShow . pointer
