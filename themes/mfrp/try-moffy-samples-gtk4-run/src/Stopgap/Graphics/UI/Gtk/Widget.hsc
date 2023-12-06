{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Widget where

import Foreign.Ptr
import Stopgap.System.GLib.Object qualified as G.Object
import Stopgap.Graphics.UI.Gtk.EventController qualified as Gtk.EventController

class G.Object.IsO w => IsW w where toW :: w -> W

data WTag

newtype W = W (Ptr WTag) deriving Show

queueDraw :: IsW w => w -> IO ()
queueDraw (toW -> W w) = c_gtk_widget_queue_draw w

foreign import ccall "gtk_widget_queue_draw" c_gtk_widget_queue_draw ::
	Ptr WTag -> IO ()

addController :: (IsW w, Gtk.EventController.IsE e) => w -> e -> IO ()
addController (toW -> W w)
	(Gtk.EventController.toE -> Gtk.EventController.E e) =
	c_gtk_widget_add_controller w e

foreign import ccall "gtk_widget_add_controller" c_gtk_widget_add_controller ::
	Ptr WTag -> Ptr Gtk.EventController.ETag -> IO ()
