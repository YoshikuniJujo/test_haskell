{-# LANGUAGE FlexibleInstances #-}

module Gtk (
	gtkInit, gtkMain, gtkMainQuit,
	gTimeoutAdd, gTimeoutAddSimple,

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
	gtkWidgetQueueDraw,
	gtkWidgetGetWindow,

	gtkContainerAdd,
	gtkWindowNew,
	gtkButtonNewWithLabel,

	gtkDrawingAreaNew,
	GdkEventKey,
	Keyval,
	char2keyval,
	gdkEventKeyGetKeyval,
	gdkEventKeyGetString,

	CairoT,
	gdkCairoCreate,
	cairoDestroy,
	cairoTranslate,
	cairoSetSourceRGB,
	cairoRectangle,
	cairoSetFontSize,
	cairoShowText,
	cairoMoveTo,
	cairoFill
) where

import Control.Applicative
import Control.Exception
import System.Environment

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

data GSourceFuncPtr
class GSourceFunc f where
	toGSourceFuncPtr :: f -> IO (FunPtr GSourceFuncPtr)
foreign import ccall "gtk/gtk.h g_timeout_add" c_gTimeoutAdd ::
	CInt -> FunPtr GSourceFuncPtr -> Ptr () -> IO ()
gTimeoutAdd :: (GSourceFunc f, Pointable p) => Int -> f -> p -> IO ()
gTimeoutAdd int fun dat = do
	pfun <- toGSourceFuncPtr fun
	pdat <- toNullPointer dat
	c_gTimeoutAdd (fromIntegral int) pfun pdat
gTimeoutAddSimple :: GSourceFunc f => Int -> f -> IO ()
gTimeoutAddSimple int fun = do
	pfun <- toGSourceFuncPtr fun
	c_gTimeoutAdd (fromIntegral int) pfun nullPtr

foreign import ccall "wrapper" wrapPIOB ::
	(Ptr () -> IO Bool) -> IO (FunPtr (Ptr () -> IO Bool))
instance Pointable p => GSourceFunc (p -> IO Bool) where
	toGSourceFuncPtr f = castFunPtr <$> wrapPIOB (\p -> do
		v <- fromNullPointer p
		f v)
foreign import ccall "wrapper" wrapIOB :: (IO Bool) -> IO (FunPtr (IO Bool))
instance GSourceFunc (IO Bool) where
	toGSourceFuncPtr f = castFunPtr <$> wrapIOB f
