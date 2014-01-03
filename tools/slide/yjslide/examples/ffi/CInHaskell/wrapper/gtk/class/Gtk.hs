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

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

import GObject
import GtkWidget
import GtkBin

foreign import ccall "gtk/gtk.h gtk_init" c_gtkInit ::
	Ptr CInt -> Ptr (Ptr CString) -> IO ()
foreign import ccall "gtk/gtk.h gtk_main" gtkMain :: IO ()
foreign import ccall "gtk/gtk.h gtk_main_quit" gtkMainQuit :: IO()

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
