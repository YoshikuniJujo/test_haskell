{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable,
	FlexibleInstances #-}

module GtkWidget (
	GtkWidget,
	gtkWidgetToGObject,
	gtkWidgetFromGObject,

	gtkWidgetShow,
	gtkWidgetShowAll,
	gtkWidgetQueueDraw,

	gtkWidgetGetWindow,
	gtkWidgetGetState,
	gtkWidgetGetStyle,

	module GtkObject,
	module GdkDrawable,
) where

import Control.Applicative

import Foreign.Ptr
import Foreign.C.Types

import GtkObject
import GdkDrawable
import GtkStyle

gClass "GtkObject" "GtkWidget"

foreign import ccall "gtk/gtk.h gtk_widget_show" c_gtkWidgetShow ::
	Ptr GtkWidget -> IO ()
foreign import ccall "gtk/gtk.h gtk_widget_show_all" c_gtkWidgetShowAll ::
	Ptr GtkWidget -> IO ()
gtkWidgetShow, gtkWidgetShowAll :: GtkWidget -> IO ()
gtkWidgetShow = c_gtkWidgetShow . pointer
gtkWidgetShowAll = c_gtkWidgetShowAll . pointer

foreign import ccall "gtk/gtk.h gtk_widget_queue_draw" c_gtkWidgetQueueDraw ::
	Ptr GtkWidget -> IO ()
gtkWidgetQueueDraw :: GtkWidget -> IO ()
gtkWidgetQueueDraw = c_gtkWidgetQueueDraw . pointer

foreign import ccall "gtk/gtk.h gtk_widget_get_window" c_gtkWidgetGetWindow ::
	Ptr GtkWidget -> IO (Ptr SomeGdkWindow)
gtkWidgetGetWindow :: GtkWidget -> IO SomeGdkWindow
gtkWidgetGetWindow gw = SomeGdkWindow <$> c_gtkWidgetGetWindow (pointer gw)

foreign import ccall "gtk/gtk.h gtk_widget_get_state" c_gtkWidgetGetState ::
	Ptr GtkWidget -> IO CInt
gtkWidgetGetState :: GtkWidget -> IO GtkStateType
gtkWidgetGetState gw = GtkStateType <$> c_gtkWidgetGetState (pointer gw)

foreign import ccall "gtk/gtk.h gtk_widget_get_style" c_gtkWidgetGetStyle ::
	Ptr GtkWidget -> IO (Ptr SomeGtkStyle)
gtkWidgetGetStyle :: GtkWidget -> IO SomeGtkStyle
gtkWidgetGetStyle gw = SomeGtkStyle <$> c_gtkWidgetGetStyle (pointer gw)

foreign import ccall "wrapper" wrapWIO ::
	(Ptr GtkWidget -> IO ()) -> IO (FunPtr (Ptr GtkWidget -> IO ()))
instance GCallback (GtkWidget -> IO ()) where
	gCallbackPtr f = castFunPtr <$> wrapWIO (f . fromPointer)

foreign import ccall "wrapper" wrapWDIO ::
	(Ptr GtkWidget -> Ptr () -> IO ()) ->
	IO (FunPtr (Ptr GtkWidget -> Ptr () -> IO ()))
instance Pointable p => GCallback (GtkWidget -> p -> IO ()) where
	gCallbackPtr f = castFunPtr <$> wrapWDIO
		(\pc pp -> do
			p <- fromNullPointer pp
			f (fromPointer pc) p)
