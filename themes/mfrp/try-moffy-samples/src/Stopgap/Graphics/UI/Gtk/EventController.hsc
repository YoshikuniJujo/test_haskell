{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.EventController where

import Foreign.Ptr

import Stopgap.System.GLib.Object qualified as G.Object

data ETag

newtype E = E (Ptr ETag) deriving Show

class G.Object.IsO e => IsE e where toE :: e -> E
