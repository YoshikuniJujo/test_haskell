{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module Gtk (
	GtkWidget,
	GtkContainer,
	SomeGtkWindow(..),
	SomeGtkButton(..),
	SomeGtkDrawingArea(..),

	gtkInit,
	gtkMain,
	gtkMainQuit,

	gSignalConnect,
	gtkWidgetShow,
	gtkWidgetShowAll,
	gdkWindow,
	gtkStyle,
	gtkWidgetState,

	gtkContainerAdd,
	gtkWindowNew,
	gtkButtonNewWithLabel,

	fgGC,
	gdkDrawPoint,
	gdkDrawRectangle,

	gtkDrawingAreaNew,

	castGObject,
) where

import System.Environment
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Exception
import Foreign.Marshal
import Foreign.Storable

import GObject
import GtkWidget
import GtkContainer
import GtkDrawingArea
import GtkStyle
import GdkDrawable

foreign import ccall "gtk/gtk.h gtk_init" c_gtkInit ::
	Ptr CInt -> Ptr (Ptr CString) -> IO ()
foreign import ccall "gtk/gtk.h gtk_main" c_gtkMain :: IO ()
foreign import ccall "gtk/gtk.h gtk_main_quit" c_gtkMainQuit ::
	Ptr GtkWidget -> Ptr () -> IO ()

withCStrings :: [String] -> ([CString] -> IO ()) -> IO ()
withCStrings str f = bracket (mapM newCString str) (mapM_ free) f

gtkInit, gtkMain :: IO ()
gtkInit = do
	prog <- getProgName
	args <- getArgs
	let allArgs = prog : args
	allocaArray (length allArgs) $ \ptr -> alloca $ \argc -> do
		withCStrings allArgs $ \cstrs -> do
			pokeArray ptr cstrs
			poke argc $ fromIntegral $ length allArgs
			alloca $ \pargv -> do
				poke pargv ptr
				c_gtkInit argc pargv
gtkMain = c_gtkMain

gtkMainQuit :: GtkWidget -> Ptr () -> IO ()
gtkMainQuit = c_gtkMainQuit . pointer
