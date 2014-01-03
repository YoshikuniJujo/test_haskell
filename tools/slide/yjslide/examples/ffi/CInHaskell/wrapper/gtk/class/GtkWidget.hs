{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module GtkWidget (
	GtkWidget(..),
	gtkWidgetShow
) where

import Foreign.Ptr
import GtkObject

data GtkWidgetPtr
class GtkWidget w where
	gtkWidgetPtr :: w -> Ptr GtkWidgetPtr
instance GtkWidget w => GtkObject w where
	gtkObjectPtr = castPtr . gtkWidgetPtr

foreign import ccall "gtk/gtk.h gtk_widget_show" c_gtkWidgetShow ::
	Ptr GtkWidgetPtr -> IO ()

gtkWidgetShow :: GtkWidget w => w -> IO ()
gtkWidgetShow w = c_gtkWidgetShow (gtkWidgetPtr w)
