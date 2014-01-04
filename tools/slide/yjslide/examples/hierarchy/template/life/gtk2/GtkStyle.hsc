#include <gtk/gtk.h>

{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GtkStyle (
	SomeGtkStyle(..),

	GtkStateType(..),
	gtkStateNormal,
	gtkStateActive,
	gtkStatePrelight,
	gtkStateSelected,
	gtkStateInsensitive,
	gtkStateInconsistent,
	gtkStateFocused
) where

import Foreign.C.Types
import GObject

gClass "GObject" "GtkStyle"

data GtkStateType = GtkStateType CInt deriving Show

#enum GtkStateType, GtkStateType, \
	GTK_STATE_NORMAL, \
	GTK_STATE_ACTIVE, \
	GTK_STATE_PRELIGHT, \
	GTK_STATE_SELECTED, \
	GTK_STATE_INSENSITIVE, \
	GTK_STATE_INCONSISTENT, \
	GTK_STATE_FOCUSED
