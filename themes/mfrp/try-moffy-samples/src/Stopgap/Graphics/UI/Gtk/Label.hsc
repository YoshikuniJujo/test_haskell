{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Label where

import Foreign.Ptr
import Foreign.C.String
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.System.GLib.Object qualified as G.Object

data LTag

newtype L = L (Ptr LTag) deriving Show

instance IsPtr L where type Tag L = LTag; fromPtr = L; toPtr (L p) = p
instance G.Object.IsO L where toO (L p) = G.Object.O $ castPtr p
instance Gtk.Widget.IsW L where toW (L p) = Gtk.Widget.W $ castPtr p

new :: String -> IO L
new txt = L <$> withCString txt c_gtk_label_new

foreign import ccall "gtk_label_new" c_gtk_label_new ::
	CString -> IO (Ptr LTag)
