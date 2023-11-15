{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Button where

import Foreign.Ptr
import Foreign.C.String
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget

data BTag

data B = B (Ptr BTag) deriving Show

instance Gtk.Widget.IsW B where
	toW (B p) = Gtk.Widget.W $ castPtr p

instance IsPtr B where
	type Tag B = BTag
	fromPtr = B
	toPtr (B p) = p

newWithLabel :: String -> IO B
newWithLabel lbl = B <$> withCString lbl c_gtk_button_new_with_label

foreign import ccall "gtk_button_new_with_label" c_gtk_button_new_with_label ::
	CString -> IO (Ptr BTag)
