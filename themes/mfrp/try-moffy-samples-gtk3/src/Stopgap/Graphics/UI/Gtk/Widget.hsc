{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Widget where

import Foreign.Ptr
import Stopgap.System.GLib.Object qualified as G.Object
import Stopgap.Graphics.UI.Gdk.Event qualified as Gdk.Event

class G.Object.IsO w => IsW w where toW :: w -> W

data WTag

newtype W = W (Ptr WTag) deriving Show

showAll :: IsW w => w -> IO ()
showAll = c_gtk_widget_show_all . toW

foreign import ccall "gtk_widget_show_all" c_gtk_widget_show_all :: W -> IO ()

addEvents :: IsW w => w -> Gdk.Event.Mask -> IO ()
addEvents = c_gtk_widget_add_events . toW

foreign import ccall "gtk_widget_add_events" c_gtk_widget_add_events ::
	W -> Gdk.Event.Mask -> IO ()
