{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkContainer (
	gtkContainerAdd,
	gtkWindowNew,
	gtkButtonNewWithLabel,
	module GtkWidget,
) where

import Control.Applicative
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import GtkWidget

gClass "GtkWidget" "GtkContainer"
foreign import ccall "gtk/gtk.h gtk_container_add" c_gtkContainerAdd ::
	Ptr GtkContainer -> Ptr GtkWidget -> IO ()
gtkContainerAdd :: GtkContainer -> GtkWidget -> IO ()
gtkContainerAdd c w = c_gtkContainerAdd (pointer c) (pointer w)

gClass "GtkContainer" "GtkBin"

gClass "GtkBin" "GtkWindow"
foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)
gtkWindowNew :: IO SomeGtkWindow
gtkWindowNew = SomeGtkWindow <$> c_gtkWindowNew 0

gClass "GtkBin" "GtkButton"
foreign import ccall "gtk/gtk.h gtk_button_new_with_label"
	c_gtkButtonNewWithLabel :: CString -> IO (Ptr SomeGtkButton)
gtkButtonNewWithLabel :: String -> IO SomeGtkButton
gtkButtonNewWithLabel s = withCString s $ \cs ->
	SomeGtkButton <$> c_gtkButtonNewWithLabel cs
