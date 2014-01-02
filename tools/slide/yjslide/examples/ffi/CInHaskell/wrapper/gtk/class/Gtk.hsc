#include <gtk/gtk.h>

{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Gtk (
	gtkInit,
	gtkMain,
	gtkMainQuit,
	gSignalConnect,
	gtkWindowNew, gtkWindowToplevel, gtkWindowPopup,
	gtkWidgetShow
) where

import System.Environment
import Control.Exception
import Control.Applicative

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "gtk/gtk.h gtk_init" c_gtkInit ::
	Ptr CInt -> Ptr (Ptr CString) -> IO ()
foreign import ccall "gtk/gtk.h gtk_main" c_gtkMain :: IO ()
foreign import ccall "gtk/gtk.h gtk_main_quit" c_gtkMainQuit :: IO()

withCStrings :: [String] -> ([CString] -> IO ()) -> IO ()
withCStrings str f = bracket (mapM newCString str) (mapM_ free) f

gtkInit, gtkMain, gtkMainQuit :: IO ()
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
gtkMainQuit = c_gtkMainQuit

data GCallbackPtr
class GCallback c where
	gCallbackPtr :: c -> IO (FunPtr GCallbackPtr)

foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))

instance GCallback (IO ()) where
	gCallbackPtr io = castFunPtr <$> wrapIO io

data GObjectPtr
class GObject o where
	gObjectPtr :: o -> Ptr GObjectPtr

foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnect ::
	Ptr GObjectPtr -> CString -> FunPtr GCallbackPtr -> Ptr () ->
	Ptr () -> Ptr () -> IO ()

gSignalConnect :: (GObject o, GCallback c) => o -> String -> c -> IO ()
gSignalConnect o s c = withCString s $ \cs -> do
	cb <- gCallbackPtr c
	c_gSignalConnect (gObjectPtr o) cs cb nullPtr nullPtr nullPtr

data GtkObjectPtr
class GtkObject o where
	gtkObjectPtr :: o -> Ptr GtkObjectPtr
instance GtkObject o => GObject o where
	gObjectPtr = castPtr . gtkObjectPtr

data GtkWidgetPtr
class GtkWidget w where
	gtkWidgetPtr :: w -> Ptr GtkWidgetPtr
instance GtkWidget w => GtkObject w where
	gtkObjectPtr = castPtr . gtkWidgetPtr

foreign import ccall "gtk/gtk.h gtk_widget_show" c_gtkWidgetShow ::
	Ptr GtkWidgetPtr -> IO ()

gtkWidgetShow :: GtkWidget w => w -> IO ()
gtkWidgetShow w = c_gtkWidgetShow (gtkWidgetPtr w)

data GtkWindowPtr
class GtkWindow w where
	gtkWindowPointer :: w -> Ptr GtkWindowPtr
instance GtkWindow w => GtkWidget w where
	gtkWidgetPtr w = castPtr $ gtkWindowPointer w

foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)

data SomeGtkWindow = SomeGtkWindow (Ptr SomeGtkWindow) deriving Show

data GtkWindowType = GtkWindowType CInt deriving Show

#enum GtkWindowType, GtkWindowType, \
	GTK_WINDOW_TOPLEVEL, \
	GTK_WINDOW_POPUP

gtkWindowNew :: GtkWindowType -> IO SomeGtkWindow
gtkWindowNew (GtkWindowType t) = SomeGtkWindow <$> c_gtkWindowNew t

instance GtkWindow SomeGtkWindow where
	gtkWindowPointer (SomeGtkWindow p) = castPtr p
