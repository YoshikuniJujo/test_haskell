module Values (GtkWindowType(..), gtkWindowToplevel, gtkWindowPopup) where

import Data.Word

#include <gtk/gtk.h>

gTK_WINDOW_TOPLEVEL, gTK_WINDOW_POPUP :: #type GtkWindowType
gTK_WINDOW_TOPLEVEL = #const GTK_WINDOW_TOPLEVEL
gTK_WINDOW_POPUP = #const GTK_WINDOW_POPUP

newtype GtkWindowType = GtkWindowType #{type GtkWindowType} deriving Show

#enum GtkWindowType, GtkWindowType, GTK_WINDOW_TOPLEVEL, GTK_WINDOW_POPUP
