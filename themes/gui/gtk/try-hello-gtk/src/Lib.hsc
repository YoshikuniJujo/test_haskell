{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (GtkWidget, gtkInit, gtkWindowNew, gtkWidgetShowAll, gtkMain) where

#include <gtk/gtk.h>

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Word

import Values

data GtkWidget = GtkWidget (Ptr GtkWidget)

foreign import ccall "gtk_init" c_gtk_init :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
foreign import ccall "gtk_window_new" c_gtk_window_new :: #{type GtkWindowType} -> IO (Ptr GtkWidget)
foreign import ccall "gtk_widget_show_all" c_gtk_widget_show_all :: Ptr GtkWidget -> IO ()
foreign import ccall "gtk_main" c_gtk_main :: IO ()
foreign import ccall "gtk_main_quit" c_gtk_main_quit :: IO ()

-- foreign import ccall "g_signal_connect" c_g_signal_connect ::
--	Ptr GtkWidget -> CString -> FunPtr (Ptr a -> IO ()) -> Ptr a -> IO ()

-- foreign import ccall "wrapper" g_callback :: (Ptr a -> IO()) -> IO (FunPtr (Ptr a -> IO ()))

foreign import ccall "hello_main" c_hello_main :: IO ()

-- gSignalConnect :: GtkWidget -> String -> (a -> IO ()) -> a -> IO ()
-- gSignalConnect

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
