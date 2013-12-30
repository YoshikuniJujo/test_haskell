{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

module Hierarchy (
	SomeGObject(..),
	GObject(..),
	Pointer(..),

	castGObject,

	GtkObject(..),
	gtkObjectToGObject,
	gtkObjectFromGObject,

	GtkWidget(..),
	SomeGtkWidget(..),
	gtkWidgetToGObject,
	gtkWidgetFromGObject,

	GtkContainer(..),
	gtkContainerToGObject,
	gtkContainerFromGObject,

	GtkBin(..),
	gtkBinToGObject,
	gtkBinFromGObject,

	GtkButton(..),
	SomeGtkButton(..),
	gtkButtonToGObject,
	gtkButtonFromGObject,

	GtkWindow(..),
	SomeGtkWindow(..),
	gtkWindowToGObject,
	gtkWindowFromGObject,
) where

import Data.Typeable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.Environment
import Foreign.Marshal
import Foreign.Storable
import Control.Applicative

data SomeGObject = forall g . GObject g => SomeGObject g deriving Typeable

class (Typeable g, Pointer g) => GObject g where
	toGObject :: g -> SomeGObject
	fromGObject :: SomeGObject -> Maybe g

	toGObject = SomeGObject
	fromGObject (SomeGObject g) = cast g

class Pointer p where
	pointer :: p -> Ptr p
	fromPointer :: Ptr p -> p

data GtkObject = forall g . GObject g => GtkObject g deriving Typeable

instance Pointer GtkObject where
	pointer (GtkObject g) = castPtr $ pointer g

instance GObject GtkObject

gtkObjectToGObject :: GObject g => g -> SomeGObject
gtkObjectToGObject = toGObject . GtkObject

gtkObjectFromGObject :: GObject g => SomeGObject -> Maybe g
gtkObjectFromGObject g = do
	GtkObject gtk <- fromGObject g
	cast gtk

data GtkWidget = forall g . GObject g => GtkWidget g deriving Typeable
data SomeGtkWidget = SomeGtkWidget (Ptr SomeGtkWidget) deriving Typeable

instance Pointer GtkWidget where
	pointer (GtkWidget g) = castPtr $ pointer g
	fromPointer p = GtkWidget $ fromPointer $ (castPtr p :: Ptr SomeGtkWidget)

instance GObject GtkWidget where
	toGObject = gtkObjectToGObject
	fromGObject = gtkObjectFromGObject

gtkWidgetToGObject :: GObject g => g -> SomeGObject
gtkWidgetToGObject = toGObject . GtkWidget

gtkWidgetFromGObject :: GObject g => SomeGObject -> Maybe g
gtkWidgetFromGObject g = do
	GtkWidget w <- fromGObject g
	cast w

instance Pointer SomeGtkWidget where
	pointer (SomeGtkWidget p) = p
	fromPointer = SomeGtkWidget

instance GObject SomeGtkWidget where
	toGObject = gtkWidgetToGObject
	fromGObject = gtkWidgetFromGObject

data GtkContainer = forall g . GObject g => GtkContainer g deriving Typeable

instance Pointer GtkContainer where
	pointer (GtkContainer g) = castPtr $ pointer g

instance GObject GtkContainer where
	toGObject = gtkWidgetToGObject
	fromGObject = gtkWidgetFromGObject

gtkContainerToGObject :: GObject g => g -> SomeGObject
gtkContainerToGObject = toGObject . GtkContainer

gtkContainerFromGObject :: GObject g => SomeGObject -> Maybe g
gtkContainerFromGObject g = do
	GtkContainer c <- fromGObject g
	cast c

data GtkBin = forall g . GObject g => GtkBin g deriving Typeable

instance Pointer GtkBin where
	pointer (GtkBin g) = castPtr $ pointer g

instance GObject GtkBin where
	toGObject = gtkContainerToGObject
	fromGObject = gtkContainerFromGObject

gtkBinToGObject :: GObject g => g -> SomeGObject
gtkBinToGObject = toGObject . GtkBin

gtkBinFromGObject :: GObject g => SomeGObject -> Maybe g
gtkBinFromGObject g = do
	GtkBin b <- fromGObject g
	cast b

data GtkButton = forall g . GObject g => GtkButton g deriving Typeable

instance Pointer GtkButton where
	pointer (GtkButton g) = castPtr $ pointer g

instance GObject GtkButton where
	toGObject = gtkBinToGObject
	fromGObject = gtkBinFromGObject

gtkButtonToGObject :: GObject g => g -> SomeGObject
gtkButtonToGObject = toGObject . GtkButton

gtkButtonFromGObject :: GObject g => SomeGObject -> Maybe g
gtkButtonFromGObject g = do
	GtkButton b <- fromGObject g
	cast b

data SomeGtkButton = SomeGtkButton (Ptr SomeGtkButton) deriving Typeable

instance Pointer SomeGtkButton where
	pointer (SomeGtkButton p) = p

instance GObject SomeGtkButton where
	toGObject = gtkButtonToGObject
	fromGObject = gtkButtonFromGObject

data GtkWindow = forall g . GObject g => GtkWindow g deriving Typeable

instance Pointer GtkWindow where
	pointer (GtkWindow g) = castPtr $ pointer g

instance GObject GtkWindow where
	toGObject = gtkBinToGObject
	fromGObject = gtkBinFromGObject

gtkWindowToGObject :: GObject g => g -> SomeGObject
gtkWindowToGObject = toGObject . GtkWindow

gtkWindowFromGObject :: GObject g => SomeGObject -> Maybe g
gtkWindowFromGObject g = do
	GtkWindow w <- fromGObject g
	cast w

data SomeGtkWindow = SomeGtkWindow (Ptr SomeGtkWindow) deriving Typeable

instance Pointer SomeGtkWindow where
	pointer (SomeGtkWindow p) = p

instance GObject SomeGtkWindow where
	toGObject = gtkWindowToGObject
	fromGObject = gtkWindowFromGObject

foreign import ccall "gtk/gtk.h gtk_init" c_gtkInit ::
	Ptr CInt -> Ptr CString -> IO ()

foreign import ccall "gtk/gtk.h gtk_main" c_gtkMain :: IO ()

foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr SomeGtkWindow)

gtkWindowNew :: CInt -> IO SomeGtkWindow
gtkWindowNew n = SomeGtkWindow <$> c_gtkWindowNew n

foreign import ccall "gtk/gtk.h gtk_button_new_with_label" c_gtkButtonNewWithLabel ::
	CString -> IO (Ptr SomeGtkButton)

gtkButtonNewWithLabel :: CString -> IO SomeGtkButton
gtkButtonNewWithLabel l = SomeGtkButton <$> c_gtkButtonNewWithLabel l

foreign import ccall "gtk/gtk.h gtk_widget_show_all" c_gtkWidgetShow ::
	Ptr GtkWidget -> IO ()

gtkWidgetShow :: GtkWidget -> IO ()
gtkWidgetShow = c_gtkWidgetShow . pointer

foreign import ccall "gtk/gtk.h gtk_container_add" c_gtkContainerAdd ::
	Ptr GtkContainer -> Ptr GtkWidget -> IO ()

gtkContainerAdd :: GtkContainer -> GtkWidget -> IO ()
gtkContainerAdd c w = c_gtkContainerAdd (pointer c) (pointer w)

foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnectData ::
	Ptr GtkWidget -> CString -> FunPtr (Ptr GtkWidget -> Ptr () -> IO ()) ->
		Ptr () -> Ptr () -> CInt -> IO ()

gSignalConnectData ::
	GtkWidget -> CString -> (GtkWidget -> Ptr () -> IO ()) ->
		Ptr () -> Ptr () -> CInt -> IO ()
gSignalConnectData w s f p1 p2 i = do
	cb <- wrapCallback (f . fromPointer)
	c_gSignalConnectData (pointer w) s cb p1 p2 i

foreign import ccall "gtk/gtk.h &gtk_main_quit" c_gtkMainQuitPtr ::
	FunPtr (Ptr GtkWidget -> Ptr () -> IO ())

foreign import ccall "gtk/gtk.h gtk_main_quit" c_gtkMainQuit ::
	Ptr GtkWidget -> Ptr () -> IO ()

gtkMainQuit :: GtkWidget -> Ptr () -> IO ()
gtkMainQuit = c_gtkMainQuit . pointer

foreign import ccall "wrapper" wrapCallback ::
	(Ptr GtkWidget -> Ptr () -> IO ()) ->
		IO (FunPtr (Ptr GtkWidget -> Ptr () -> IO ()))

castGObject :: (GObject g, GObject h) => g -> Maybe h
castGObject = fromGObject . toGObject

main :: IO ()
main = do
	args <- getArgs
	allocaArray (length args) $ \ptr -> alloca $ \argc -> do
		mapM newCString args >>= pokeArray ptr
		poke argc $ fromIntegral $ length args
		c_gtkInit argc ptr
		win <- gtkWindowNew 0
		button <- gtkButtonNewWithLabel =<< newCString "Button"
		destroy <- newCString "destroy"
		clicked <- newCString "clicked"
		case (castGObject win, castGObject win, castGObject button) of
			(Just w, Just wc, Just b) -> do
				gtkContainerAdd wc b
				gSignalConnectData b clicked gtkMainQuit
					nullPtr nullPtr 0
				gSignalConnectData w destroy gtkMainQuit
					nullPtr nullPtr 0
				gtkWidgetShow w
				c_gtkMain
			_ -> error "bad"
