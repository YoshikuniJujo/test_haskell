{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable,
	FlexibleInstances #-}

module GtkWidget (
	GtkWidget,
	gtkWidgetShow,
	gtkWidgetShowAll,
	gtkWidgetToGObject,
	gtkWidgetFromGObject,
	module GtkObject,
) where

import Control.Applicative

import Foreign.Ptr

import GtkObject

gClass "GtkObject" "GtkWidget"
foreign import ccall "gtk/gtk.h gtk_widget_show" c_gtkWidgetShow ::
	Ptr GtkWidget -> IO ()
foreign import ccall "gtk/gtk.h gtk_widget_show_all" c_gtkWidgetShowAll ::
	Ptr GtkWidget -> IO ()
gtkWidgetShow, gtkWidgetShowAll :: GtkWidget -> IO ()
gtkWidgetShow = c_gtkWidgetShow . pointer
gtkWidgetShowAll = c_gtkWidgetShowAll . pointer

foreign import ccall "wrapper" wrapWDIO ::
	(Ptr GtkWidget -> Ptr () -> IO ()) ->
	IO (FunPtr (Ptr GtkWidget -> Ptr () -> IO ()))
instance Pointable p => GCallback (GtkWidget -> p -> IO ()) where
	gCallbackPtr f = castFunPtr <$> wrapWDIO
		(\pc pp -> do
			p <- fromNullPointer pp
			f (fromPointer pc) p)
