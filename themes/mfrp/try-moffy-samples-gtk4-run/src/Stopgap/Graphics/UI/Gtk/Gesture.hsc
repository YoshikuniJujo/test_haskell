{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Gesture where

import Foreign.Ptr
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.EventController qualified as Gtk.EventController
import Stopgap.System.GLib.Object qualified as G.Object

data GTag

newtype G = G (Ptr GTag) deriving Show

class Gtk.EventController.IsE g => IsG g where toG :: g -> G

instance IsPtr G where type Tag G = GTag; fromPtr = G; toPtr (G p) = p
instance G.Object.IsO G where toO (G g) = G.Object.O $ castPtr g

instance Gtk.EventController.IsE G where
	toE (G g) = Gtk.EventController.E $ castPtr g
