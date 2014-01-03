{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkContainer (
	gtkWindowNew,
	module GtkWidget,
) where

import Control.Applicative
import Foreign.Ptr
import Foreign.C.Types

import GtkWidget

gClass "GtkWidget" "GtkContainer"

gClass "GtkContainer" "GtkBin"

gClass "GtkBin" "GtkWindow"
foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)
gtkWindowNew :: IO SomeGtkWindow
gtkWindowNew = SomeGtkWindow <$> c_gtkWindowNew 0
