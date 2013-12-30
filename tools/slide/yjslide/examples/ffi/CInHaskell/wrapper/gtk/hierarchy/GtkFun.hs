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
) where

import Data.Typeable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.Environment
import Foreign.Marshal
import Foreign.Storable
import Control.Applicative

import Hierarchy

foreign import ccall "gtk/gtk.h gtk_init" c_gtkInit ::
	Ptr CInt -> Ptr CString -> IO ()

foreign import ccall "gtk/gtk.h gtk_main" c_gtkMain :: IO ()

foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)

gtkWindowNew :: CInt -> IO SomeGtkWindow
gtkWindowNew n = SomeGtkWindow <$> c_gtkWindowNew n

foreign import ccall "gtk/gtk.h gtk_button_new_with_label" c_gtkButtonNewWithLabel ::
	CString -> IO (Ptr SomeGtkButton)

gtkButtonNewWithLabel :: CString -> IO SomeGtkButton
gtkButtonNewWithLabel l = SomeGtkButton <$> c_gtkButtonNewWithLabel l

foreign import ccall "gtk/gtk.h gtk_widget_show_all" c_gtkWidgetShow ::
	Ptr GtkWidget -> IO ()

gtkWidgetShow :: GtkWidget -> IO ()
gtkWidgetShow = c_gtkWidgetShow . pointer

foreign import ccall "gtk/gtk.h gtk_container_add" c_gtkContainerAdd ::
	Ptr GtkContainer -> Ptr GtkWidget -> IO ()

gtkContainerAdd :: GtkContainer -> GtkWidget -> IO ()
gtkContainerAdd c w = c_gtkContainerAdd (pointer c) (pointer w)

foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnectData ::
	Ptr GtkWidget -> CString -> FunPtr (Ptr GtkWidget -> Ptr () -> IO ()) ->
		Ptr () -> Ptr () -> CInt -> IO ()

gSignalConnectData ::
	GtkWidget -> CString -> (GtkWidget -> Ptr () -> IO ()) ->
		Ptr () -> Ptr () -> CInt -> IO ()
gSignalConnectData w s f p1 p2 i = do
	cb <- wrapCallback (f . fromPointer)
	c_gSignalConnectData (pointer w) s cb p1 p2 i

foreign import ccall "gtk/gtk.h &gtk_main_quit" c_gtkMainQuitPtr ::
	FunPtr (Ptr GtkWidget -> Ptr () -> IO ())

foreign import ccall "gtk/gtk.h gtk_main_quit" c_gtkMainQuit ::
	Ptr GtkWidget -> Ptr () -> IO ()

gtkMainQuit :: GtkWidget -> Ptr () -> IO ()
gtkMainQuit = c_gtkMainQuit . pointer

foreign import ccall "wrapper" wrapCallback ::
	(Ptr GtkWidget -> Ptr () -> IO ()) ->
		IO (FunPtr (Ptr GtkWidget -> Ptr () -> IO ()))
