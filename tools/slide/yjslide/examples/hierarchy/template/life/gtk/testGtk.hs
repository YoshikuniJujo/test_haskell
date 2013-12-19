{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

import System.Environment
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Exception
import Foreign.Marshal
import Foreign.Storable
import Control.Applicative

import LifeTemplate
import GObject

gClass "GObject" "GtkObject"
gClass "GtkObject" "GtkWidget"
gClass "GtkWidget" "GtkContainer"
gClass "GtkContainer" "GtkBin"
gClass "GtkBin" "GtkButton"
gClass "GtkBin" "GtkWindow"

foreign import ccall "gtk/gtk.h gtk_init" c_gtkInit ::
	Ptr CInt -> Ptr (Ptr CString) -> IO ()
foreign import ccall "gtk/gtk.h gtk_main" c_gtkMain :: IO ()
foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)
foreign import ccall "gtk/gtk.h gtk_widget_show" c_gtkWidgetShow ::
	Ptr GtkWidget -> IO ()
foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnectData ::
	Ptr GtkWidget -> CString -> FunPtr (Ptr GtkWidget -> Ptr () -> IO ()) ->
		Ptr () -> Ptr () -> CInt -> IO ()
foreign import ccall "gtk/gtk.h gtk_main_quit" c_gtkMainQuit ::
	Ptr GtkWidget -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapCallback ::
	(Ptr GtkWidget -> Ptr () -> IO ()) ->
		IO (FunPtr (Ptr GtkWidget -> Ptr () -> IO ()))

withCStrings :: [String] -> ([CString] -> IO ()) -> IO ()
withCStrings str f = bracket (mapM newCString str) (mapM_ free) f
withCStrings' str f = bracket (mapM newCString str) (const $ return ()) f

allocaArray' :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray' n f = do
	ptr <- mallocArray n
	f ptr

alloca' :: Storable a => (Ptr a -> IO b) -> IO b
alloca' f = do
	ptr <- malloc
	f ptr

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

gtkWindowNew :: IO SomeGtkWindow
gtkWindowNew = SomeGtkWindow <$> c_gtkWindowNew 0

gtkWidgetShow :: GtkWidget -> IO ()
gtkWidgetShow = c_gtkWidgetShow . pointer

gSignalConnect :: GtkWidget -> String -> (GtkWidget -> Ptr () -> IO ()) -> IO ()
gSignalConnect w s f = do
	cb <- wrapCallback (f . fromPointer)
	cs <- newCString s
	c_gSignalConnectData (pointer w) cs cb nullPtr nullPtr 0

main = do
	gtkInit
	win <- gtkWindowNew
	case (castGObject win) of
		Just w -> do
			gtkWidgetShow w
			gSignalConnect w "destroy" gtkMainQuit
			gtkMain
		_ -> error "bad"
