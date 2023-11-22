{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
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

import Graphics.CairoType

import Graphics.Gtk(gtkInit, gtkMain, gtkMainQuit)

#include <gtk/gtk.h>

newtype GtkWindow = GtkWindow (Ptr GtkWindow) deriving Show

instance Pointer GtkWindow where
	pointer (GtkWindow p) = ($ p)
	modifyPointer (GtkWindow p) f = GtkWindow $ f p

gObjectHierarchy Nothing $ GObjectNode "GInitialUnowned" [
	GObjectNode "GtkWidget" [
		GObjectNode "GtkContainer" [
			GObjectNode "GtkBin" [GObjectType ''GtkWindow] ] ] ]

newtype GtkDrawingArea = GtkDrawingArea (Ptr GtkDrawingArea) deriving Show

gObjectHierarchy (Just ''GtkWidget) $ GObjectType ''GtkDrawingArea

instance Pointer GtkDrawingArea where
	pointer (GtkDrawingArea p) = ($ p)
	modifyPointer (GtkDrawingArea p) f = GtkDrawingArea $ f p

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
	da <- gtkDrawingAreaNew
	gSignalConnect da DrawEvent (\w cr () -> False <$ print (w, cr)) ()
--	gSignalConnect da DrawEvent (\w cr () -> False <$ putStrLn "foo") ()
--	gSignalConnect da DrawEvent (\w cr () -> False <$ print w) ()
--	gSignalConnect da DrawEvent (\w cr () -> False <$ print cr) ()
	gtkContainerAdd w da
	gtkWidgetShowAll w
	gtkMain

gtkWidgetShowAll :: GObject o => o -> IO ()
gtkWidgetShowAll o = do
	o' <- gCastObjectIo o
	pointer o' c_gtk_widget_show_all

foreign import ccall "gtk_widget_show_all" c_gtk_widget_show_all :: Ptr GtkWidget -> IO ()

data DeleteEvent = DeleteEvent deriving Show

instance Signal DeleteEvent where
	type Reciever DeleteEvent = GtkWidget
	type Callback DeleteEvent o a = o -> GdkEvent -> a -> IO Bool
	type CCallback DeleteEvent o a = Ptr o -> Ptr GdkEvent -> Ptr a -> IO #type gboolean
	signalName DeleteEvent = "delete-event"
	callbackToCCallback = callbackToCCallbackDeleteEvent
	wrapCCallback = c_wrapper_delete_event

newtype GdkEvent = GdkEvent (Ptr GdkEvent) deriving Show

boolToGBoolean :: Bool -> #type gboolean
boolToGBoolean False = #const FALSE
boolToGBoolean True = #const TRUE

callbackToCCallbackDeleteEvent :: (Pointer o, AsPointer a) => o -> Callback DeleteEvent o a -> CCallback DeleteEvent o a
callbackToCCallbackDeleteEvent o c pw pe px = do
	x <- asValue px
	boolToGBoolean <$> c (modifyPointer o $ const pw) (GdkEvent pe) x

foreign import ccall "wrapper" c_wrapper_delete_event ::
--	(CCallback DeleteEvent o a) -> IO (FunPtr (CCallback DeleteEvent o a))
	(Ptr o -> Ptr GdkEvent -> Ptr a -> IO Int32) ->
	IO (FunPtr (Ptr o -> Ptr GdkEvent -> Ptr a -> IO Int32))

data DrawEvent = DrawEvent deriving Show

instance Signal DrawEvent where
	type Reciever DrawEvent = GtkWidget
	type Callback DrawEvent o a = o -> CairoT -> a -> IO Bool
	type CCallback DrawEvent o a = Ptr o -> Ptr CairoT -> Ptr a -> IO #type gboolean
	signalName DrawEvent = "draw"
	callbackToCCallback = callbackToCCallbackDrawEvent
	wrapCCallback = c_wrapper_draw_event

callbackToCCallbackDrawEvent :: (Pointer o, AsPointer a) => o ->Callback DrawEvent o a -> CCallback DrawEvent o a
callbackToCCallbackDrawEvent o c pw pe px = do
	x <- asValue px
	boolToGBoolean <$> c (modifyPointer o $ const pw) (CairoT pe) x

foreign import ccall "wrapper" c_wrapper_draw_event ::
--	(CCallback DrawEvent o a) -> IO (FunPtr (CCallback DrawEvent o a))
	(Ptr o -> Ptr CairoT -> Ptr a -> IO Int32) ->
	IO (FunPtr (Ptr o -> Ptr CairoT -> Ptr a -> IO Int32))

foreign import ccall "gtk_drawing_area_new" c_gtk_drawing_area_new :: IO (Ptr GtkDrawingArea)

foreign import ccall "gtk_container_add" c_gtk_container_add ::
	Ptr GtkContainer -> Ptr GtkWidget -> IO ()

gtkDrawingAreaNew :: IO GtkDrawingArea
gtkDrawingAreaNew = GtkDrawingArea <$> c_gtk_drawing_area_new

gtkContainerAdd :: (GObject c, GObject w) => c -> w -> IO ()
gtkContainerAdd c w = do
	c' <- gCastObjectIo c
	w' <- gCastObjectIo w
	pointer c' \pc -> pointer w' \pw -> c_gtk_container_add pc pw
