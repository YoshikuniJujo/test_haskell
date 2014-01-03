#include <gtk/gtk.h>

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module GtkWindow (
	gtkWindowNew, gtkWindowToplevel, gtkWindowPopup,
) where

import Control.Applicative

import Foreign.Ptr
import Foreign.C.Types

import GtkBin

data GtkWindowPtr
class GtkWindow w where
	gtkWindowPtr :: w -> Ptr GtkWindowPtr
instance GtkWindow w => GtkBin w where
	gtkBinPtr w = castPtr $ gtkWindowPtr w

foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)

data SomeGtkWindow = SomeGtkWindow (Ptr SomeGtkWindow) deriving Show

data GtkWindowType = GtkWindowType CInt deriving Show
#enum GtkWindowType, GtkWindowType, GTK_WINDOW_TOPLEVEL, GTK_WINDOW_POPUP

gtkWindowNew :: GtkWindowType -> IO SomeGtkWindow
gtkWindowNew (GtkWindowType t) = SomeGtkWindow <$> c_gtkWindowNew t

instance GtkWindow SomeGtkWindow where
	gtkWindowPtr (SomeGtkWindow p) = castPtr p
