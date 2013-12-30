{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

#include "gtk/gtk.h"

module GtkWidget (
	GtkWidget,
	gtkWidgetToGObject,
	gtkWidgetFromGObject,

	gSignalConnect,
	gtkWidgetShow,
	gtkWidgetShowAll,
) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable

import GObject
import GdkDrawable
import GtkStyle

gClass "GObject" "GtkObject"
gClass "GtkObject" "GtkWidget"
gClass "GtkWidget" "GtkContainer"
gClass "GtkContainer" "GtkBin"
gClass "GtkBin" "GtkButton"
gClass "GtkBin" "GtkWindow"
gClass "GtkWidget" "GtkDrawingArea"

foreign import ccall "wrapper" wrapCallback ::
	(Ptr GtkWidget -> Ptr () -> IO ()) ->
		IO (FunPtr (Ptr GtkWidget -> Ptr () -> IO ()))

foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnectData ::
	Ptr GtkWidget -> CString -> FunPtr (Ptr GtkWidget -> Ptr () -> IO ()) ->
		Ptr () -> Ptr () -> CInt -> IO ()

foreign import ccall "gtk/gtk.h gtk_widget_show" c_gtkWidgetShow ::
	Ptr GtkWidget -> IO ()
foreign import ccall "gtk/gtk.h gtk_widget_show_all" c_gtkWidgetShowAll ::
	Ptr GtkWidget -> IO ()

gtkWidgetShow, gtkWidgetShowAll :: GtkWidget -> IO ()
gtkWidgetShow = c_gtkWidgetShow . pointer
gtkWidgetShowAll = c_gtkWidgetShowAll . pointer

gSignalConnect :: GtkWidget -> String -> (GtkWidget -> Ptr () -> IO ()) -> IO ()
gSignalConnect w s f = do
	cb <- wrapCallback (f . fromPointer)
	cs <- newCString s
	c_gSignalConnectData (pointer w) cs cb nullPtr nullPtr 0

c_gdkWindow :: Ptr GtkWidget -> IO (Ptr SomeGdkWindow)
c_gdkWindow = #peek GtkWidget, window

c_gtkStyle :: Ptr GtkWidget -> IO (Ptr SomeGtkStyle)
c_gtkStyle = #peek GtkWidget, style
