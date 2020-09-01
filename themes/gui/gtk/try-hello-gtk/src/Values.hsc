module Values (GtkWindowType(..), gtkWindowToplevel, gtkWindowPopup) where

import Data.Word

#include <gtk/gtk.h>

newtype GtkWindowType = GtkWindowType #{type GtkWindowType} deriving Show

#enum GtkWindowType, GtkWindowType, GTK_WINDOW_TOPLEVEL, GTK_WINDOW_POPUP
