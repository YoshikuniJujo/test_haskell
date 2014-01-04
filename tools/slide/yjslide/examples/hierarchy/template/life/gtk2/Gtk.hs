module Gtk (
	gtkInit, gtkMain, gtkMainQuit,

	GObject,
	GClosure,
	pointer,
	castGObject,
	gSignalConnect,
	gSignalConnectData,
	zero,

	GtkWidget,
	gtkWidgetShow,
	gtkWidgetShowAll,
	gtkWidgetGetWindow,

	gtkContainerAdd,
	gtkWindowNew,
	gtkButtonNewWithLabel,

	gtkDrawingAreaNew,

	gdkCairoCreate,
	cairoRectangle,
	cairoFill
) where

import System.Environment
import Control.Exception

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

import GtkContainer
import GtkDrawingArea

foreign import ccall "gtk/gtk.h gtk_init" c_gtkInit ::
	Ptr CInt -> Ptr (Ptr CString) -> IO ()
foreign import ccall "gtk/gtk.h gtk_main" gtkMain :: IO ()
foreign import ccall "gtk/gtk.h gtk_main_quit" gtkMainQuit :: IO ()

withCStrings :: [String] -> ([CString] -> IO ()) -> IO ()
withCStrings str = bracket (mapM newCString str) (mapM_ free)

gtkInit :: IO ()
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
