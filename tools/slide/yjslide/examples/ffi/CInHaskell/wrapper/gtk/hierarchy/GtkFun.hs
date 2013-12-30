{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

module GtkFun (
	c_gtkInit,
	c_gtkMain,
	c_gtkWindowNew,
	c_gtkButtonNewWithLabel,
	c_gtkWidgetShow,
	c_gtkContainerAdd,
	c_gSignalConnectData,
	c_gtkMainQuit,
	wrapCallback,
	wrapDestructor
) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Hierarchy

foreign import ccall "gtk/gtk.h gtk_init" c_gtkInit ::
	Ptr CInt -> Ptr CString -> IO ()

foreign import ccall "gtk/gtk.h gtk_main" c_gtkMain :: IO ()

foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)

foreign import ccall "gtk/gtk.h gtk_button_new_with_label" c_gtkButtonNewWithLabel ::
	CString -> IO (Ptr SomeGtkButton)

foreign import ccall "gtk/gtk.h gtk_widget_show_all" c_gtkWidgetShow ::
	Ptr GtkWidget -> IO ()

foreign import ccall "gtk/gtk.h gtk_container_add" c_gtkContainerAdd ::
	Ptr GtkContainer -> Ptr GtkWidget -> IO ()

foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnectData ::
	Ptr GtkWidget -> CString -> FunPtr (Ptr GtkWidget -> Ptr () -> IO ()) ->
		Ptr () -> FunPtr (Ptr () -> Ptr () -> IO ()) -> CInt -> IO ()

foreign import ccall "gtk/gtk.h &gtk_main_quit" c_gtkMainQuitPtr ::
	FunPtr (Ptr GtkWidget -> Ptr () -> IO ())

foreign import ccall "gtk/gtk.h gtk_main_quit" c_gtkMainQuit ::
	Ptr GtkWidget -> Ptr () -> IO ()

foreign import ccall "wrapper" wrapCallback ::
	(Ptr GtkWidget -> Ptr () -> IO ()) ->
		IO (FunPtr (Ptr GtkWidget -> Ptr () -> IO ()))

foreign import ccall "wrapper" wrapDestructor ::
	(Ptr () -> Ptr () -> IO ()) ->
		IO (FunPtr (Ptr () -> Ptr () -> IO ()))
