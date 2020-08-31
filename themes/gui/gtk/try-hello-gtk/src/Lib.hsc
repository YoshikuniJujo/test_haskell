{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (
	GtkWidget, gtkInit, gtkWindowNew, gtkWidgetShowAll, gtkMain,
	Event, AsPointer, gSignalConnect, Destroy(..), gtkMainQuit,
	KeyPressEvent(..), 
	) where

#include <gtk/gtk.h>

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Word

import Values

newtype GtkWidget = GtkWidget (Ptr GtkWidget) deriving Show

class AsPointer a where
	asPointer :: a -> (Ptr a -> IO b) -> IO b

instance AsPointer GtkWidget where
	asPointer (GtkWidget p) f = f p

instance Storable a => AsPointer a where
	asPointer x f = alloca \p -> do
		poke p x
		f p

foreign import ccall "gtk_init" c_gtk_init :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
foreign import ccall "gtk_window_new" c_gtk_window_new :: #{type GtkWindowType} -> IO (Ptr GtkWidget)
foreign import ccall "gtk_widget_show_all" c_gtk_widget_show_all :: Ptr GtkWidget -> IO ()
foreign import ccall "gtk_main" c_gtk_main :: IO ()
foreign import ccall "gtk_main_quit" c_gtk_main_quit :: IO ()

gtkMainQuit :: IO ()
gtkMainQuit = c_gtk_main_quit

{-
type Handler e a = Ptr GtkWidget -> Ptr e -> Ptr a -> IO ()
data GdkEventKey = GdkEventKey (Ptr GdkEventKey)
-}

class Event e where
	type Handler e a
	eventName :: e -> String
	g_callback :: Handler e a -> IO (FunPtr (Handler e a))

data Destroy = Destroy deriving Show
instance Event Destroy where
	type Handler Destroy a = IO ()
	eventName Destroy = "destroy"
	g_callback = g_callback0

data KeyPressEvent = KeyPressEvent deriving Show
instance Event KeyPressEvent where
	type Handler KeyPressEvent a = GtkWidget -> GdkEventKey -> Ptr a -> IO ()
	eventName KeyPressEvent = "key-press-event"
	g_callback = g_callback_keypress
newtype GdkEventKey = GdkEventKey (Ptr GdkEventKey) deriving Show

foreign import capi "gtk/gtk.h g_signal_connect" c_g_signal_connect ::
	Ptr GtkWidget -> CString -> FunPtr () -> Ptr a -> IO ()

-- foreign import ccall "wrapper" g_callback :: Handler e a -> IO (FunPtr (Handler e a))
foreign import ccall "wrapper" g_callback0 :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "wrapper" g_callback_keypress ::
	(GtkWidget -> GdkEventKey -> Ptr a -> IO ()) -> IO (FunPtr (GtkWidget -> GdkEventKey -> Ptr a -> IO ()))

foreign import ccall "hello_main" c_hello_main :: IO ()

gSignalConnect :: forall e a . (Event e, AsPointer a) => GtkWidget -> e -> Handler e a -> a -> IO ()
gSignalConnect (GtkWidget pw) e h x = do
	cs <- newCString $ eventName e
	cb <- castFunPtr <$> g_callback @e @a h
	asPointer x $ c_g_signal_connect pw cs cb
--	alloca \p -> do
--		poke p x
--		c_g_signal_connect pw cs cb p

{-
gSignalConnect :: Storable a => GtkWidget -> String -> (Handler e a) -> a -> IO ()
gSignalConnect (GtkWidget pw) en h x = do
	cs <- newCString en
	cb <- g_callback h
	alloca \p -> do
		poke p x
		c_g_signal_connect pw cs cb p
		-}

gtkInit :: [String] -> IO [String]
gtkInit as = allocaArray (length as) \arr -> do
	cas <- newCString `mapM` as
	pokeArray arr cas
	(n', arr') <- alloca \pn -> do
		poke pn . fromIntegral $ length as
		arr' <- alloca \parr -> do
			poke parr arr
			c_gtk_init pn parr
			peek parr
		(, arr') <$> peek pn
	(peekCString `mapM`) =<< peekArray (fromIntegral n') arr'

gtkWindowNew :: GtkWindowType -> IO GtkWidget
gtkWindowNew (GtkWindowType wt) = GtkWidget <$> c_gtk_window_new wt

gtkWidgetShowAll :: GtkWidget -> IO ()
gtkWidgetShowAll (GtkWidget pw) = c_gtk_widget_show_all pw

gtkMain :: IO ()
gtkMain = c_gtk_main
