{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Window where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application

data WTag

data W = W (Ptr WTag) deriving Show

new :: IO W
new = W <$> c_gtk_window_new

foreign import ccall "gtk_window_new" c_gtk_window_new :: IO (Ptr WTag)

setApplication :: W -> Gtk.Application.A -> IO ()
setApplication (W w) (Gtk.Application.A a) = c_gtk_window_set_application w a

foreign import ccall "gtk_window_set_application"
	c_gtk_window_set_application ::
	Ptr WTag -> Ptr (Gtk.Application.ATag) -> IO ()

present :: W -> IO ()
present (W w) = c_gtk_window_present w

foreign import ccall "gtk_window_present" c_gtk_window_present ::
	Ptr WTag -> IO ()

setTitle :: W -> String -> IO ()
setTitle (W w) ttl = withCString ttl $ c_gtk_window_set_title w

foreign import ccall "gtk_window_set_title" c_gtk_window_set_title ::
	Ptr WTag -> CString -> IO ()

setDefaultSize :: W -> CInt -> CInt -> IO ()
setDefaultSize (W win) w h = c_gtk_window_set_default_size win w h

foreign import ccall "gtk_window_set_default_size"
	c_gtk_window_set_default_size :: Ptr WTag -> CInt -> CInt -> IO ()
