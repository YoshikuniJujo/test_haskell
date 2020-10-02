{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Gobject.TryDeleteEvent where

import Foreign.Ptr
import Foreign.Tools
import Data.Word
import Data.Int
import System.Gobject.Hierarchy
import System.Gobject.SignalConnect

import Graphics.Gtk(gtkInit, gtkMain, gtkMainQuit)

#include <gtk/gtk.h>

newtype GtkWindow = GtkWindow (Ptr GtkWindow) deriving Show

instance Pointer GtkWindow where
	pointer (GtkWindow p) = ($ p)
	value = GtkWindow

gObjectHierarchy Nothing $ GObjectNode "GInitialUnowned" [
	GObjectNode "GtkWidget" [
		GObjectNode "GtkContainer" [
			GObjectNode "GtkBin" [GObjectType ''GtkWindow] ] ] ]

gtkWindowNew :: GtkWindowType -> IO GtkWindow
gtkWindowNew (GtkWindowType wt) = GtkWindow <$> c_gtk_window_new wt

foreign import ccall "gtk_window_new" c_gtk_window_new :: #{type GtkWindowType} -> IO (Ptr GtkWindow)

newtype GtkWindowType = GtkWindowType #{type GtkWindowType} deriving Show

#enum GtkWindowType, GtkWindowType, GTK_WINDOW_TOPLEVEL, GTK_WINDOW_POPUP

tryDeleteEvent :: IO ()
tryDeleteEvent = do
	[] <- gtkInit []
	w <- gtkWindowNew gtkWindowToplevel
	gSignalConnect w DeleteEvent (\_ _ () -> True <$ (putStrLn "delete-event" >> gtkMainQuit)) ()
	gtkWidgetShowAll w
	gtkMain

gtkWidgetShowAll :: GObject o => o -> IO ()
gtkWidgetShowAll o = case gCastObject o of
	Nothing -> error "boooo"
	Just o' -> pointer o' c_gtk_widget_show_all

foreign import ccall "gtk_widget_show_all" c_gtk_widget_show_all :: Ptr GtkWidget -> IO ()

data DeleteEvent = DeleteEvent deriving Show

instance Signal DeleteEvent where
	type Reciever DeleteEvent = GtkWidget
	type Callback DeleteEvent a = Ptr () -> a -> IO Bool
	type CCallback DeleteEvent a = Ptr () -> Ptr a -> IO #type gboolean
	signalName DeleteEvent = "delete-event"
	callbackToCCallback = callbackToCCallbackDeleteEvent
	wrapCCallback = c_wrapper_delete_event

boolToGBoolean :: Bool -> #type gboolean
boolToGBoolean False = #const FALSE
boolToGBoolean True = #const TRUE

callbackToCCallbackDeleteEvent :: AsPointer a => Callback DeleteEvent a -> CCallback DeleteEvent a
callbackToCCallbackDeleteEvent c pe px = do
	x <- asValue px
	boolToGBoolean <$> c pe x

foreign import ccall "wrapper" c_wrapper_delete_event ::
	(Ptr (Reciever DeleteEvent) -> CCallback DeleteEvent a) -> IO (FunPtr (Ptr (Reciever DeleteEvent) -> CCallback DeleteEvent a))
