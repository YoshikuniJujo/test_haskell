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

foreign import ccall "wrapper" wrapCallback ::
	(Ptr GtkWidget -> Ptr () -> IO ()) ->
		IO (FunPtr (Ptr GtkWidget -> Ptr () -> IO ()))

foreign import ccall "gtk/gtk.h gtk_init" c_gtkInit ::
	Ptr CInt -> Ptr (Ptr CString) -> IO ()
foreign import ccall "gtk/gtk.h gtk_main" c_gtkMain :: IO ()
foreign import ccall "gtk/gtk.h gtk_main_quit" c_gtkMainQuit ::
	Ptr GtkWidget -> Ptr () -> IO ()
foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnectData ::
	Ptr GtkWidget -> CString -> FunPtr (Ptr GtkWidget -> Ptr () -> IO ()) ->
		Ptr () -> Ptr () -> CInt -> IO ()

foreign import ccall "gtk/gtk.h gtk_widget_show" c_gtkWidgetShow ::
	Ptr GtkWidget -> IO ()
foreign import ccall "gtk/gtk.h gtk_widget_show_all" c_gtkWidgetShowAll ::
	Ptr GtkWidget -> IO ()

foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)
foreign import ccall "gtk/gtk.h gtk_button_new_with_label" c_gtkButtonNewWithLabel ::
	CString -> IO (Ptr SomeGtkButton)

foreign import ccall "gtk/gtk.h gtk_container_add" c_gtkContainerAdd ::
	Ptr GtkContainer -> Ptr GtkWidget -> IO ()

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

gtkButtonNewWithLabel :: String -> IO SomeGtkButton
gtkButtonNewWithLabel l = do
	cl <- newCString l
	SomeGtkButton <$> c_gtkButtonNewWithLabel cl

gtkWidgetShow, gtkWidgetShowAll :: GtkWidget -> IO ()
gtkWidgetShow = c_gtkWidgetShow . pointer
gtkWidgetShowAll = c_gtkWidgetShowAll . pointer

gSignalConnect :: GtkWidget -> String -> (GtkWidget -> Ptr () -> IO ()) -> IO ()
gSignalConnect w s f = do
	cb <- wrapCallback (f . fromPointer)
	cs <- newCString s
	c_gSignalConnectData (pointer w) cs cb nullPtr nullPtr 0

gtkContainerAdd :: GtkContainer -> GtkWidget -> IO ()
gtkContainerAdd c w = c_gtkContainerAdd (pointer c) (pointer w)

main = do
	gtkInit
	win <- gtkWindowNew
	btn <- gtkButtonNewWithLabel "button"
	case (castGObject win, castGObject win, castGObject btn) of
		(Just w, Just cw, Just b) -> do
			gtkContainerAdd cw b
			gtkWidgetShowAll w
			gSignalConnect w "destroy" gtkMainQuit
			gSignalConnect b "clicked" gtkMainQuit
			gtkMain
		_ -> error "bad"
