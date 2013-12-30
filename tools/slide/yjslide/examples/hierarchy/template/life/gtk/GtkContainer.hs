{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkContainer (
	GtkContainer,
	SomeGtkWindow(..),
	SomeGtkButton(..),

	gtkContainerAdd,
	gtkWindowNew,
	gtkButtonNewWithLabel,
) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Applicative

import GObject
import GtkWidget

gClass "GtkWidget" "GtkContainer"
gClass "GtkContainer" "GtkBin"
gClass "GtkBin" "GtkButton"
gClass "GtkBin" "GtkWindow"

foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)
foreign import ccall "gtk/gtk.h gtk_button_new_with_label" c_gtkButtonNewWithLabel ::
	CString -> IO (Ptr SomeGtkButton)

foreign import ccall "gtk/gtk.h gtk_container_add" c_gtkContainerAdd ::
	Ptr GtkContainer -> Ptr GtkWidget -> IO ()

gtkWindowNew :: IO SomeGtkWindow
gtkWindowNew = SomeGtkWindow <$> c_gtkWindowNew 0

gtkButtonNewWithLabel :: String -> IO SomeGtkButton
gtkButtonNewWithLabel l = do
	cl <- newCString l
	SomeGtkButton <$> c_gtkButtonNewWithLabel cl

gtkContainerAdd :: GtkContainer -> GtkWidget -> IO ()
gtkContainerAdd c w = c_gtkContainerAdd (pointer c) (pointer w)
