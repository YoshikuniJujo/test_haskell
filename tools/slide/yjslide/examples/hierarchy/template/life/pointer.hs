{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

import Foreign.Ptr
import LifeTemplate
import GObject

gClass "GObject" "GtkObject"
gClass "GtkObject" "GtkWidget"
gClass "GtkWidget" "GtkContainer"
gClass "GtkContainer" "GtkBin"
gClass "GtkBin" "GtkButton"
gClass "GtkBin" "GtkWindow"
