{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Window where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.Application qualified as Gtk.Application
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget

class Gtk.Widget.IsW a => IsW a where toW :: a -> W

data WTag

data W = W (Ptr WTag) deriving Show

instance IsPtr W where type Tag W = WTag; fromPtr = W; toPtr (W p) = p
instance Gtk.Widget.IsW W where toW (W w) = Gtk.Widget.W $ castPtr w
instance IsW W where toW = id

new :: IO W
new = W <$> c_gtk_window_new

foreign import ccall "gtk_window_new" c_gtk_window_new :: IO (Ptr WTag)

setApplication :: IsW w => w -> Gtk.Application.A s -> IO ()
setApplication (toW -> W w) (Gtk.Application.A a) = c_gtk_window_set_application w a

foreign import ccall "gtk_window_set_application"
	c_gtk_window_set_application ::
	Ptr WTag -> Ptr (Gtk.Application.ATag) -> IO ()

present :: IsW w => w -> IO ()
present (toW -> W w) = c_gtk_window_present w

foreign import ccall "gtk_window_present" c_gtk_window_present ::
	Ptr WTag -> IO ()

setTitle :: IsW w => w -> String -> IO ()
setTitle (toW -> W w) ttl = withCString ttl $ c_gtk_window_set_title w

foreign import ccall "gtk_window_set_title" c_gtk_window_set_title ::
	Ptr WTag -> CString -> IO ()

setDefaultSize :: IsW w => w -> CInt -> CInt -> IO ()
setDefaultSize (toW -> W win) w h = c_gtk_window_set_default_size win w h

foreign import ccall "gtk_window_set_default_size"
	c_gtk_window_set_default_size :: Ptr WTag -> CInt -> CInt -> IO ()

setChild :: (IsW w, Gtk.Widget.IsW c) => w -> c -> IO ()
setChild (toW -> W win) (Gtk.Widget.toW -> Gtk.Widget.W cld) =
	c_gtk_window_set_child win cld

foreign import ccall "gtk_window_set_child" c_gtk_window_set_child ::
	Ptr WTag -> Ptr Gtk.Widget.WTag -> IO ()

destroy :: IsW w => w -> IO ()
destroy (toW -> W win) = c_gtk_window_destroy win

foreign import ccall "gtk_window_destroy" c_gtk_window_destroy ::
	Ptr WTag -> IO ()
