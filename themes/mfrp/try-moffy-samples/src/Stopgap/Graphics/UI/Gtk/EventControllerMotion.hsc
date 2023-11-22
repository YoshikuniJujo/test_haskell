{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.EventControllerMotion where

import Foreign.Ptr
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.EventController qualified as Gtk.EventController
import Stopgap.System.GLib.Object qualified as G.Object

data ETag

newtype E = E (Ptr ETag) deriving Show

instance IsPtr E where type Tag E = ETag; fromPtr = E; toPtr (E p) = p
instance G.Object.IsO E where toO (E e) = G.Object.O $ castPtr e

instance Gtk.EventController.IsE E where
	toE (E e) = Gtk.EventController.E $ castPtr e

new :: IO E
new = E <$> c_gtk_event_controller_motion_new

foreign import ccall "gtk_event_controller_motion_new"
	c_gtk_event_controller_motion_new :: IO (Ptr ETag)
